{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Clone a github repository into a cache directory.
module Foliage.GitClone (
  gitClone,
  addGitCloneRule,
)
where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Foliage.Meta (GitHubRepo, GitHubRev)
import GHC.Generics (Generic)

data GitClone = GitClone {repo :: GitHubRepo, rev :: GitHubRev}
  deriving (Eq, Generic, NFData)

instance Show GitClone where
  show GitClone{repo, rev} = "gitClone " <> show repo <> " " <> show rev

instance Hashable GitClone

instance Binary GitClone

type instance RuleResult GitClone = FilePath

-- | Clone given repo at given revision into the cache directory and return the working copy path.
gitClone :: GitHubRepo -> GitHubRev -> Action FilePath
gitClone repo rev = apply1 GitClone{repo, rev}

-- | Set up the 'GitClone' rule with a cache directory.
addGitCloneRule
  :: FilePath
  -- ^ Cache directory
  -> Rules ()
addGitCloneRule cacheDir = addBuiltinRule noLint noIdentity run
 where
  run :: BuiltinRun GitClone FilePath
  run GitClone{repo, rev} _old _mode = do
    let path = cacheDir </> "git" </> show repo

    alreadyCloned <- doesDirectoryExist path
    if alreadyCloned
      then command_ [Cwd path] "git" ["fetch"]
      else do
        let url = "https://github.com/" <> show repo <> ".git"
        command_ [] "git" ["clone", "--recursive", url, path]

    command_ [Cwd path] "git" ["checkout", show rev]
    command_ [Cwd path] "git" ["submodule", "update"]

    return $ RunResult{runChanged = ChangedRecomputeDiff, runStore = "", runValue = path}
