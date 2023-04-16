#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cabal-install git nix
set -o errexit
set -o nounset
set -o pipefail

# README Quickstart example
# https://github.com/andreabedini/foliage#quickstart
mkdir -p Quickstart/_sources/typed-protocols/0.1.0.0
cat > Quickstart/_sources/typed-protocols/0.1.0.0/meta.toml << EOF
github = { repo = "input-output-hk/ouroboros-network", rev = "fa10cb4eef1e7d3e095cec3c2bb1210774b7e5fa" }
subdir = "typed-protocols"
timestamp = 2022-03-29T06:19:50+00:00
EOF
nix run ".#" -- build --input-directory Quickstart/_sources --output-directory Quickstart/_repo
INDEX_TARBALL="Quickstart/_repo/01-index.tar"
EXPECTED_CONTENT=("typed-protocols/0.1.0.0/typed-protocols.cabal" "typed-protocols/0.1.0.0/package.json")
tar -tf "$INDEX_TARBALL" > index_contents.txt
for expected in "${EXPECTED_CONTENT[@]}"; do
  if ! grep -q "$expected" index_contents.txt; then
    echo "Error: $expected not found in index tarball"
    exit 1
  fi
done

# Cardano Haskell package repository ("CHaP")
# https://github.com/input-output-hk/cardano-haskell-packages
git clone https://github.com/input-output-hk/cardano-haskell-packages CHaP
nix run ".#" -- build -j 0 --write-metadata --input-directory CHaP/_sources --output-directory CHaP/_repo
cabal build cardano-prelude
