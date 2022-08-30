{
  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) lib;

        plan = lib.importJSON ./dist-newstyle/cache/plan.json;

        # whatever, we do have the right versions in the plan
        cabal-install = pkgs.cabal-install;
        ghc = pkgs.haskell.compiler.ghc8107;

        fetchHackageSdist = { package, version, sha256 }:
          builtins.fetchurl {
            inherit sha256;
            url =
              "https://hackage.haskell.org/package/${package}-${version}/${package}-${version}.tar.gz";
            name = "${package}-${version}.tar.gz";
          };

        fetchCabalFileByHash = { package, version, sha256 }:
          builtins.fetchurl {
            inherit sha256;
            url = "https://casa.fpcomplete.com/${sha256}";
            name = "${package}-${version}-cabal-file-${sha256}";
          };

        fetchComponentSrc =
          { pkg-name, pkg-version, pkg-src, pkg-src-sha256, pkg-cabal-sha256 }:
          if pkg-src.type == "repo-tar" then
            builtins.fetchurl {
              url =
                "${pkg-src.repo.uri}package/${pkg-name}-${pkg-version}/${pkg-name}-${pkg-version}.tar.gz";
              name = "${pkg-name}-${pkg-version}.tar.gz";
              sha256 = pkg-src-sha256;
            }
          else if pkg-src.type == "local" then
          # simplifying (and terrifying) assumption
            ./.
          else
            abort "boh";

        inherit (plan) install-plan;

        install-plan-map =
          lib.mapAttrs (_: builtins.head) (lib.groupBy (p: p.id) install-plan);

        externalDeps = [ pkgs.zlib ];

        buildComponent = n:

          let
            all-depends = builtins.concatMap (n:
              (lib.optionals (n ? depends) n.depends)
              ++ (lib.optionals (n ? exe-depends) n.exe-depends)) ([ n ]
                ++ lib.optionals (n ? components)
                (builtins.attrValues n.components));

          in pkgs.stdenv.mkDerivation {
            name = n.id;

            propagatedBuildInputs = externalDeps ++ builtins.map buildComponent
              (builtins.filter (n: n.type == "configured")
                (builtins.map (d: install-plan-map.${d}) all-depends));

            setupHook = pkgs.writeScript "setup-hook.sh" ''
              ghcPackagesHook() {
                if [[ -d "$1/packages.conf" ]]; then
                  if [[ ! -d $out/packages.conf ]]; then
                    mkdir -p $out/packages.conf
                  fi
                  # collect all the package specification files
                  for n in $1/packages.conf/*.conf; do
                    cp --no-clobber $n $out/packages.conf
                  done
                fi
              }

              addEnvHooks "$targetOffset" ghcPackagesHook
            '';

            postUnpack = if n ? pkg-cabal-sha256 then ''
              echo "Updating ${n.pkg-name}.cabal with the revision ${n.pkg-cabal-sha256}"
              cp ${
                fetchCabalFileByHash {
                  package = n.pkg-name;
                  version = n.pkg-version;
                  sha256 = n.pkg-cabal-sha256;
                }
              } $sourceRoot/${n.pkg-name}.cabal
            '' else
              "";

            configurePhase = let
              components =
                map (c: if c == "lib" then "lib:" + n.pkg-name else c)
                (if n ? component-name then
                  [ n.component-name ]
                else if n ? components then
                  (builtins.attrNames n.components)
                else
                  abort "boh");

            in ''
              runHook preConfigure

              mkdir -p $out

              if [[ ! -d $out/packages.conf ]]; then
                mkdir $out/packages.conf
              fi
              ${ghc}/bin/ghc-pkg --package-db $out/packages.conf recache
              ${ghc}/bin/ghc-pkg --package-db $out/packages.conf check

              if grep 'build-type:\s\+Configure' ${n.pkg-name}.cabal; then
                export BUILD_TYPE=Configure
              else
                export BUILD_TYPE=Simple
              fi

              ${cabal-install}/bin/cabal act-as-setup \
                --build-type=$BUILD_TYPE              \
                configure --                          \
                --verbose=2                           \
                --cid=${n.id}                         \
                --prefix $out                         \
                --package-db=$out/packages.conf       \
                --with-compiler=${ghc}/bin/ghc        \
                ${builtins.concatStringsSep " " components}

              runHook postConfigure
            '';

            buildPhase = ''
              runHook preBuild
              ${cabal-install}/bin/cabal act-as-setup build
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              ${cabal-install}/bin/cabal act-as-setup install
              runHook postInstall
            '';

            src = fetchComponentSrc {
              inherit (n)
                pkg-name pkg-version pkg-src pkg-src-sha256 pkg-cabal-sha256;
            };
          };

        allPackages = lib.mapAttrs (_: buildComponent) install-plan-map;

        localPackages = lib.mapAttrs (_: buildComponent)
          (lib.filterAttrs (n: v: v ? style && v.style == "local")
            install-plan-map);

      in {
        packages = allPackages // {
          default = builtins.head (builtins.attrValues localPackages);
        };

        devShells.default =
          pkgs.mkShell { buildInputs = [ ghc cabal-install ]; };
      });
}
