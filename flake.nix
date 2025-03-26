{
  description =
    "Foliage is a tool to create custom Haskell package repositories, in a fully reproducible way.";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, haskell-nix, ... }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
          overlays = [ haskell-nix.overlay ];
        };
        inherit (pkgs) lib;

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc96";
          shell.tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
            fourmolu = "0.14.0.0";
          };

          modules = [{
            # Wrap executables with the needed dependencies in PATH. See #71.
            packages.foliage.postInstall = ''
              for exe in $(find $out/bin -type f -executable); do
                wrapProgram "$exe" \
                    --prefix PATH : ${with pkgs; lib.makeBinPath [ coreutils curl gnutar gitMinimal patch ]}
              done
            '';
          }];
        };

        flake = project.flake (
          lib.attrsets.optionalAttrs (system == "x86_64-linux")
            { crossPlatforms = p: [ p.musl64 ]; }
        );
      in
      flake // {
        inherit project;

        apps =
          flake.apps
          // { default = flake.apps."foliage:exe:foliage"; }
          # Expose the derivation for a static executable as "static"
          // lib.attrsets.optionalAttrs (system == "x86_64-linux")
            { static = flake.apps."x86_64-unknown-linux-musl:foliage:exe:foliage"; }
          // lib.attrsets.optionalAttrs (system == "aarch64-linux")
            { static = flake.apps."aarch64-multiplatform-musl:foliage:exe:foliage"; }
        ;

        packages =
          flake.packages
          // { default = flake.packages."foliage:exe:foliage"; }

          # Expose the derivation for a static executable as "static"
          // lib.attrsets.optionalAttrs (system == "x86_64-linux")
            { static = flake.packages."x86_64-unknown-linux-musl:foliage:exe:foliage"; }
          // lib.attrsets.optionalAttrs (system == "aarch64-linux")
            { static = flake.packages."aarch64-multiplatform-musl:foliage:exe:foliage"; }
        ;
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
