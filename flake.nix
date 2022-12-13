{
  description =
    "Foliage is a tool to create custom Haskell package repositories, in a fully reproducible way.";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
          overlays = [ haskell-nix.overlay ];
        };
        project = pkgs.pkgsCross.musl64.haskell-nix.cabalProject {
          src = ./.;
          compiler-nix-name = "ghc8107";
          shell.tools = {
            cabal = { };
            hlint = { };
            haskell-language-server = { };
          };
          modules =
            [{ packages.foliage.components.exes.foliage.dontStrip = false; }];

        };
      in {
        packages.default = project.foliage.components.exes.foliage;
        hydraJobs.foliage = project.foliage.components.exes.foliage;
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://foliage.cachix.org"
      "https://ci.zw3rk.com"
     ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "foliage.cachix.org-1:kAFyYLnk8JcRURWReWZCatM9v3Rk24F5wNMpEj14Q/g="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
