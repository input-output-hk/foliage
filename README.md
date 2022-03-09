# foliage

A hash-friendly Haskell Package Repository.

Foliage is a tool to create custom or private Haskell package repositories,
in a fully reproducible way.

## Background

The problem of build reproducibility in the Haskell ecosystem has discussed
many times. Hackage does not natively offer a way to pin down the files it
serves.

Although there are workarounds to obtain a fixed repository (e.g. by
truncating the index file, which is append only) I think we can solve this
at the root.

## Main idea

_Like GitHub Pages but for Haskell Packages_

A "Hackage repository" is collection of files (source distributions, cabal
files, public keys and signatures).

These files are commonly served by Hackage proper, that is the central
deployment of [hackage-server](https://github.com/haskell/hackage-server/).

Foliage explores the idea of serving this content as a static website,
which is generated programmatically from a small set of input files.

Both the input files and the generated repository can be stored in a git
repository and referred to via stable URL corresponding to commit hashes.


## Example

An input file could look like the following

```toml
[[sources]]
url = https://..../source1.tar.gz

[[sources]]
url = https://..../source2.tar.gz
subdirs = [
    "a",
    "b",
    "c"
]
```

This file basically mirrors the functionality of
[`source-repository-package`](https://cabal.readthedocs.io/en/3.6/cabal-project.html#specifying-packages-from-remote-version-control-locations)
in Cabal.

For each source (and each subdir, if any is specified), foliage will
download the tarball and make a sdist. Foliage will then use the
hackage-repo-tool to create an on-disk repository (e.g. in `_repo`) from
the collected packages. Additionally, one can specify revisions to each
package version.

The resulting repository can then be server through HTTPS and used with
cabal, e.g. in a `cabal.project`

```
repository packages.example.org
  url: https://packages.example.org/
  secure: True
```

**Note:** The package id (package name + package version) is unknown at
download time and only known after looking at the cabal file. This is the
reason package names and versions do not show in the input file. Foliage
ensures two sources do not provide colliding package ids.

**Note:** Hackage implements [The Update
Framework](https://theupdateframework.io) which requires a set of public
and private keys. Foliage can either generate a new set of keys or reuse a
pre-existing one. Cabal can either trust a repository at first use or
verify signatures against public keys obtained separately.

## GitHub

Foliage can make use of three features supported by GitHub, to further advance automation.

1. GitHub has long suppored accessing git repositories via HTTPS. E.g. one can access a blob in a git repo through the following URL.

   https://raw.githubusercontent.com/{owner}/{repo}/{ref}/path

   where `ref` can either be a commit hash or a branch name.

2. GitHub also offer URLs of tarballs for repos at given commit, e.g.

   https://github.com/Quid2/flat/tarball/ee59880f47ab835dbd73bea0847dab7869fc20d8

   Afaik, these tarballs might not be entirely immutable (TODO)

3. GitHub offers URLs for tagged releases (these tarballs are supposed to be immutable).

4. GitHub Actions can be used to automate the generation

5. (Perhaps optional) GitHub Pages supports publishing a git branch over HTTP.

This means we automatically have a stable url for any package whose source is available on GitHub.
Also the generated repository can be committed to a git branch and be immediately available through HTTPS to cabal.

E.g.

This configuration

https://github.com/andreabedini/byo-hackage/blob/933760117a3800366b420b07c8c887c1313e2b22/packages.tsv

(warning old TSV format)

Generated this repo https://github.com/andreabedini/byo-hackage/tree/1e8c5184836acb05972dfff00ac8edca575e1df1

Which can be give to cabal like this

```
repository my-hackage-repo
  url: https://raw.githubusercontent.com/andreabedini/byo-hackage/1e8c5184836acb05972dfff00ac8edca575e1df1
  secure: True
```

## To infinity and Beyond

One can think of more features

- A pretty website could be automatically generated along with the
  repository. With a list of packages, their versions, metadata, etc
- The input file itself could be automatically generated, e.g. from all
  tagged releases in a GitHub organisation. Making it a turn-key Hackage
  repository for any GitHub Organisation.

## Author

- Andrea Bedini (@andreabedini)

