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

    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
          overlays = [ haskell-nix.overlay ];
        };

        # FIXME: It seems sadly that enabling this makes `ghc` not found in
        # $PATH on my machine, which makes indeed HLS quite unable to lint the
        # sources ...
        #
        # pkgs-static-where-possible =
        #   if pkgs.stdenv.hostPlatform.isLinux then
        #     if pkgs.stdenv.hostPlatform.isAarch64 then
        #       pkgs.pkgsCross.aarch64-multiplatform-musl
        #     else
        #       pkgs.pkgsCross.musl64
        #   else
        #     pkgs;

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc926";

          shell.tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };

        flake = project.flake { };

      in
      flake // { packages.default = flake.packages."foliage:exe:foliage"; });

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
