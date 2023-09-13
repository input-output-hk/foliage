module Foliage.Utils.GitHub (
  githubRepoTarballUrl,
)
where

import Data.Text qualified as T
import Foliage.Meta (GitHubRepo (unGitHubRepo), GitHubRev (unGitHubRev))
import Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth)
import System.FilePath ((</>))

githubRepoTarballUrl :: GitHubRepo -> GitHubRev -> URI
githubRepoTarballUrl repo rev =
  nullURI
    { uriScheme = "https:"
    , uriAuthority = Just nullURIAuth{uriRegName = "github.com"}
    , uriPath = "/" </> T.unpack (unGitHubRepo repo) </> "tarball" </> T.unpack (unGitHubRev rev)
    }
