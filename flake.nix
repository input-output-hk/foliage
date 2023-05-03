{
  description =
    "Foliage is a tool to create custom Haskell package repositories, in a fully reproducible way.";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    hackage-nix.url = "github:input-output-hk/hackage.nix";
    hackage-nix.flake = false;
    flake-utils.follows = "haskell-nix/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
          overlays = [ haskell-nix.overlay ];
        };
        inherit (pkgs) lib;

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc926";
          shell.tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };

        flake = project.flake (
          lib.attrsets.optionalAttrs (system == "x86_64-linux")
            { crossPlatforms = p: [ p.musl64 ]; }
          // lib.attrsets.optionalAttrs (system == "aarch64-linux")
            { crossPlatforms = p: [ p.aarch64-multiplatform-musl ]; }
        );
      in

      flake // {
        inherit project;

        # This is way too much boilerplate. I only want the default package to
        # be the main exe (package or app) and "static" the static version on
        # the systems where it is available.

        apps = { default = flake.apps."foliage:exe:foliage"; }
        // lib.attrsets.optionalAttrs (system == "x86_64-linux")
          { static = flake.apps."x86_64-unknown-linux-musl:foliage:exe:foliage"; }
        // lib.attrsets.optionalAttrs (system == "aarch64-linux")
          { static = flake.apps."aarch64-multiplatform-musl:foliage:exe:foliage"; };

        packages = { default = flake.packages."foliage:exe:foliage"; }
        // lib.attrsets.optionalAttrs (system == "x86_64-linux")
          { static = flake.packages."x86_64-unknown-linux-musl:foliage:exe:foliage"; }
        // lib.attrsets.optionalAttrs (system == "aarch64-linux")
          { static = flake.packages."aarch64-multiplatform-musl:foliage:exe:foliage"; };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://foliage.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "foliage.cachix.org-1:kAFyYLnk8JcRURWReWZCatM9v3Rk24F5wNMpEj14Q/g="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
