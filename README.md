# foliage

A hash-friendly Haskell Package Repository.

Foliage is a tool to create custom or private Haskell package repositories,
in a fully reproducible way.

## Main idea

_Like GitHub Pages but for Haskell Packages_

A "Hackage repository" is collection of source distributions and cabal
files. In addition, Hackage has implemented [The Update
Framework (TUF)](https://theupdateframework.com) and the repository also
includes cryptographic metadata (public keys and signatures).

These files are commonly served by Hackage proper, that is the central
deployment of [hackage-server](https://github.com/haskell/hackage-server/).

Foliage explores the idea of creaating and serving this content as a static
website, generated programmatically from textual input files.

## Example

Foliage expects a folder `_sources` with a subfolder per package name and
version.

E.g.

```
_sources
└── typed-protocols
    └── 0.1.0.0
        └── meta.toml
```

The file `meta.toml` describes a package and looks like this

```toml
timestamp = 2022-03-28T07:57:10Z
url = 'https://github.com/input-output-hk/ouroboros-network/tarball/d2d219a86cda42787325bb8c20539a75c2667132'
subdir = 'typed-protocols' # optional
```

Foliage will download the source url for each package (assumed to be a
tarball), decompress it, make a source distribution and take the cabal
file.

After all packages have been processed, foliage will create a repository,
including the index and the TUF metadata. With the input above foliage will
produce the following:

```
_repo
├── 01-index.tar
├── 01-index.tar.gz
├── index
│   └── typed-protocols
│       └── 0.1.0.0
│           ├── package.json
│           └── typed-protocols.cabal
├── mirrors.json
├── package
│   └── typed-protocols-0.1.0.0.tar.gz
├── root.json
├── snapshot.json
└── timestamp.json
```

* `typed-protocols-0.1.0.0.tar.gz` is obtained by running
  `cabal sdist` of the repository (and, optionally, subfolder) specified in
  `meta.toml`.
* `type-protocols.cabal` is extracted from the repository.
* `01-index.tar` will include the cabal files and signed target file, using
  the timestamp in `meta.toml`.
  ```bash
  $ TZ=UTC tar tvf _repo/01-index.tar
  -rw-r--r-- foliage/foliage 1627 2022-03-28 07:57 typed-protocols/0.1.0.0/typed-protocols.cabal
  -rw-r--r-- foliage/foliage  833 2022-03-28 07:57 typed-protocols/0.1.0.0/package.json
  ```
* The TUF files (`mirrors.json`, `root.json`, `snapshot.json`,
  `timestamp.json`) are signed and contains reasonable defaults.

## Revisions

Foliage supports cabal file revisions. Adding the following snippet to a
package's `meta.toml`, will make foliage look for a cabal file in
`<pkgName>/<pkgVersion>/revisions/1.cabal`.

```
[[revisions]]
  number = 1
  timestamp = 2022-03-22T14:15:00+00:00
```

The revised cabal file will enter the index with the timestamp provided in
`meta.toml`.

## Using the repository with cabal

The resulting repository can then be server through HTTPS and used with
cabal, e.g. in a `cabal.project`:

```
repository packages.example.org
  url: https://packages.example.org/
  secure: True
```

Alternatively, cabal can read the repository directly off disk:

```
repository packages.example.org
  url: file:///path/to/_repo
  secure: True
```

**Note:** Hackage implements [The Update
Framework](https://theupdateframework.io) which requires a set of public
and private keys. Foliage can either generate a new set of keys or reuse a
pre-existing one. Cabal can either trust a repository at first use or
verify signatures against public keys obtained separately.

## Author

- Andrea Bedini (@andreabedini)

