{
  description = "Foliage is a tool to create custom Haskell package repositories, in a fully reproducible way.";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; inherit (haskell-nix) config; overlays = [haskell-nix.overlay]; };
      project = pkgs.haskell-nix.cabalProject {
        src = ./.;
        compiler-nix-name = "ghc8107";
        shell.tools = {
          cabal = {};
          hlint = {};
          haskell-language-server = {};
        };
        shell.buildInputs = with pkgs; [
          nixpkgs-fmt
        ];
        modules = [{
          packages.foliage.components.exes.foliage.dontStrip = false;
        }];
      };
    in {
      packages.default = project.foliage.components.exes.foliage;

      devShell = pkgs.mkShell {
        name = "foliage-dev-shell";
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
