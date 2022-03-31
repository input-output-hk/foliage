# Notes

## sdists

Do we need to make sdists? I think so.
Does `cabal sdist` need `cabal.project`?
Perhaps using Distribution.Simple directly?
Idk, I don't understand sdists.

*Note:* perhaps include hash validation? this can be done later

## usage

### quick and easy

served through https (nginx, s3, GitHub)

```
repository packages.example.org
  url: https://packages.example.org/
  secure: True
```

Note: we don't even need GitHub Pages. A raw.githubusercontent.com is good enough for cabal.

### totally reproducible

```
repository packages.example.org
  url: https://packages.example.org/HASH
  secure: True
```

### properly secure

```
repository packages.example.org
  url: https://packages.example.org/
  secure: True
  root-keys: <root-key-IDs>
  key-threshold: <key-threshold>
```

with root-keys available from webpage

### locally available

useful for nix

```
repository my-local-repo
  url: file:/path/to/local/repo
  secure: True
```

### website

It might be nice to have a minimal website generated along the repository

Things to include:

- list of packages, with their metadata and link to cabal files
- hashes of everything, ready to cut and paste
- instructions, snipped to add to cabal.project or cabal.config
- previous deployments/versions

I would much prefer if there was a way to keep the website separate from the output above (for hash friendliness). The presentation layer can be separate. Perhaps something that can fetch the data and render on the spot? Something with Cloudflare Pages and Workers?

Maybe just a separate directory with a bunch of links, deployed on a separate branch on GitHub

Branches:
- main (the source of all evil)
- repository (the assembled repository)
- website (the rendered website, with links to the repository branch)

## References:

1. https://github.com/hvr/hackage-overlay-repo-tool/blob/master/src/Main.hs
2. https://github.com/haskell/hackage-security/blob/master/hackage-repo-tool/src/Main.hs
3. https://cabal.readthedocs.io/en/3.6/installing-packages.html#repository-specification
