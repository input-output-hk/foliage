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

Foliage explores the idea of creating and serving this content as a static
website, generated programmatically a deterministically from textual input
files.

# Use cases

## Company internal hackage

Company XYZ has developed many Haskell packages, some of which are forks of
public Haskell libraries. For various reasons XYZ might not be inclined in
publishing their packages to Hackage. If XYZ is using multiple repositories
for version control, keeping all the company's packages in sync will become
a problem.

Currently XYZ's best option is to use `cabal.project` files. Each cabal
package can declare its non-Hackage dependencies using the
[`source-repository-package` stanza](https://cabal.readthedocs.io/en/3.6/cabal-project.html#specifying-packages-from-remote-version-control-locations).

*Note*: packages can be grouped together into a cabal project, in which case
`source-repository-package` stanzas become the project dependencies; this
distintion is inconsequential to our example.

E.g. if packageA needs packageB, hosted on GitHub; packageA's
`cabal.project` will include:

```
source-repository-package
    type: git
    location: https://github.com/Company-XYZ/packageB
    tag: e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1
```

While the presence of a git tag makes this quite reproducible; a problem
arises in that these dependencies are not transitive. Without any
versioning to help, one has to manually pick a working set of dependencies.

E.g. if packageC depends on packageA, packageC `cabal.project` will have to
include something like:

```
-- Direct dependency
source-repository-package
    type: git
    location: https://github.com/Company-XYZ/packageA
    tag: e76fdc753e660dfa615af6c8b6a2ad9ddf6afe70

-- From packageA
source-repository-package
    type: git
    location: https://github.com/Company-XYZ/packageB
    tag: e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1
```

Having an internal company Hackage, solves the above problem by
reintroducing versioning and a familiar workflow for handling Hackage
dependencies; while maintaining absolute control and flexibility over
versioning policies and dependency management.

When the team behind packageA wants to push out a new version, say version
1.2.3.4, all they have to do is to update the foliage repository with a
file `packageA/1.2.3.4/meta.toml` with content:

```toml
timestamp = 2022-03-29T06:19:50+00:00
url = 'https://github.com/Company-XYZ/packageA/tarball/e76fdc753e660dfa615af6c8b6a2ad9ddf6afe70'
```

*Note*: Any other url would work here. E.g. one could use GitHub releases:
`https://github.com/Company-XYZ/packageA/archive/refs/tags/v1.2.3.4.tar.gz`.

:information_source: Although the `timestamp` field in the package source metadata is
optional, it is highly reccomended if you intend your repository users to
be able to use cabal's `index-state` functionality. Adding a timestamp
every time you add a package version ensures the newly created index is
"compatible" with what the users have already fetched.

# Quickstart

### Adding one package

It's reccomended to create a working directory first.

Let's add a package (say `typed-protocols-0.1.0.0` from https://github.com/input-output-hk/ouroboros-network/tree/master/typed-protocols, at commit hash fa10cb4eef1e7d3e095cec3c2bb1210774b7e5fa).

```bash
$ mkdir -p _sources/typed-protocols/0.1.0.0
$ cat _sources/typed-protocols/0.1.0.0/meta.toml
url = 'https://github.com/input-output-hk/ouroboros-network/tarball/fa10cb4eef1e7d3e095cec3c2bb1210774b7e5fa'
subdir = 'typed-protocols'
```

### Building the repository

Run foliage build.

```
$ foliage build
🌿 Foliage
You don't seem to have created a set of TUF keys. I will create one in _keys
Current time set to 2022-05-16T09:33:26Z. You can set a fixed time using the --current-time option.
Expiry time set to 2023-05-16T09:33:26Z (a year from now).
# curl (for RemoteAsset "https://github.com/input-output-hk/ouroboros-network/tarball/fa10cb4eef1e7d3e095cec3c2bb1210774b7e5fa")
# tar (for OracleQ (PreparePackageSource (PackageId {pkgName = "typed-protocols", pkgVersion = "0.1.0.0"})))
# cp (for OracleQ (PreparePackageSource (PackageId {pkgName = "typed-protocols", pkgVersion = "0.1.0.0"})))
# writing (for _repo/root.json)
# writing (for _repo/mirrors.json)
_cache/packages/typed-protocols/0.1.0.0
Creating source distribution for typed-protocols-0.1.0.0
# cabal (for _repo/package/typed-protocols-0.1.0.0.tar.gz)
# mv (for _repo/package/typed-protocols-0.1.0.0.tar.gz)
# writing (for _repo/01-index.tar)
# writing (for _repo/01-index.tar.gz)
# writing (for _repo/snapshot.json)
# writing (for _repo/timestamp.json)
All done. The repository is now available in _repo.
```

If you want to rely on the cabal index-state feature you need to specify a
timestamp in the `meta.toml` file.

E.g.

```
$ cat _sources/typed-protocols/0.1.0.0/meta.toml
url = 'https://github.com/input-output-hk/ouroboros-network/tarball/fa10cb4eef1e7d3e095cec3c2bb1210774b7e5fa'
subdir = 'typed-protocols'
timestamp = 2022-03-29T06:19:50+00:00
```

:information_source: Foliage uses the metadata timestamps to determine the
order of the entries in `01-index`. This allows you to create an index that
1) can be updated incrementally and 2) can be used with cabal's
[index-state](https://cabal.readthedocs.io/en/3.6/cabal-project.html?highlight=index-state#cfg-field-index-state)
feature.

With the input above foliage will produce the following:

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

The content of `_repo` can be served over HTTP(s) to cabal, e.g. from a
`cabal.project` file.

```
repository packages.example.org
  url: http://packages.example.org/
  secure: True
  root-keys:
    -- root-keys ids, see below
    144d97d34d0a86adb1ca7d6bdc1b2d9f0c9123e3c29e3765f5a9eece345ce4f9
    a15f6ae88a26638934d90eff28da29990a4b12c8bb0b2c12f07e9a510e839a97
    fde23c79a14bcbef6ccf198b4ad94ded4092784fcaed17c3d184008e9bf6f722
  key-threshold: 3
```

### TUF keys

Foliage creates a set of private keys to sign the TUF metadata at first
use. By default, the keys are created in `_keys`:

```
_keys/
├── mirrors
│   ├── 105369fb9cb1555cf4517be3e885215a7bc730bd59cf3084ea7140f9692ae847.json
│   ├── 2178cff7b2a3a6edd396371c03bc8fddb77b77b104a9fd97f6291f2c49285946.json
│   └── 4689f8a6d905a59536213a27ea577b34ff3e6d79d5a7b375458d3bb6026a5e13.json
├── root
│   ├── 144d97d34d0a86adb1ca7d6bdc1b2d9f0c9123e3c29e3765f5a9eece345ce4f9.json
│   ├── a15f6ae88a26638934d90eff28da29990a4b12c8bb0b2c12f07e9a510e839a97.json
│   └── fde23c79a14bcbef6ccf198b4ad94ded4092784fcaed17c3d184008e9bf6f722.json
├── snapshot
│   └── 07a918ccdb3ac0600a8b65f3bc96da18dfc5be65c023c64ccd0fb6a04692b64d.json
├── target
│   ├── 793b1b2730db6ec5247934ad0cc7b28ed3b05eae4ffec7e28e7b1739f1cb65b4.json
│   ├── 908041deaae700390dfd7b4c8d3eca7049c8172a205bea19de8314c4fd9ca56f.json
│   └── f102c7035da2a3b5fa82b90d8afe3e8892d13a8c6b7a4281403c340317a35014.json
└── timestamp
    └── 141da8eb2ccba61c2f6bb656b2292970d086770f5bf7d53802d2bc0ec1defa26.json
```

The root-keys ids are simply the names of they key files in
`_keys/roots/*.json`.

These keys are small enough you can store them in an environment variable.
E.g.

Save:
```bash
$ KEYS=$(tar cz -C _keys . | base64)
```

Restore:
```bash
$ mkdir _keys
$ echo "$KEYS" | base64 -d | tar xz -C _keys
```

:warning: These are private keys. Don't publish them along with your
repository!

# Revisions

Foliage supports cabal file revisions. Adding the following snippet to a
package's `meta.toml`, will make foliage look for a cabal file in
`<package>/<version>/revisions/1.cabal`.

```
[[revisions]]
  number = 1
  timestamp = 2022-03-22T14:15:00+00:00
```

The revised cabal file will enter the index with the timestamp provided in
`meta.toml`.

# Patches

Foliage also supports patches. Any file with `.patch` extension in
`<package>/<version>/patches` will be applied as a patch to the package
sources.

Patches are applied very early in the process, and they can modify any file
in the source tree.

Revisions are independent of patches, a patched cabal file will be subject
to revisions just like the original cabal file.

:warning: It is not possible to "apply a timestamp" to a patch. A patch
changes the content of the source distribution and the repository can only
provide one source distribution for a given package name and version.

# New package versions

You can introduce new versions of existing packages by patching the cabal
file. Foliage supports a short cut for this use case. If the package
metadata file `<package>/<version>/meta.toml` includes the option
`force-version = true`, then the version in the package cabal file will be
overwritten with `<version>`. Notice this is done after applying patches.

## Author

- Andrea Bedini (@andreabedini)
