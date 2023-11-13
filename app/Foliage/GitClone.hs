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
import Foliage.Meta (GitHubRepo)
import GHC.Generics (Generic)

newtype GitClone = GitClone {repo :: GitHubRepo}
  deriving (Eq, Generic)
  deriving newtype (NFData)

instance Show GitClone where
  show GitClone{repo} = "gitClone " <> show repo

instance Hashable GitClone

instance Binary GitClone

type instance RuleResult GitClone = FilePath

-- | Clone given repo at given revision into the cache directory and return the working copy path.
gitClone :: GitHubRepo -> Action FilePath
gitClone repo = apply1 GitClone{repo}

-- | Set up the 'GitClone' rule with a cache directory.
addGitCloneRule
  :: FilePath
  -- ^ Cache directory
  -> Rules ()
addGitCloneRule cacheDir = addBuiltinRule noLint noIdentity run
 where
  run :: BuiltinRun GitClone FilePath
  run GitClone{repo} _old _mode = do
    let path = cacheDir </> "git" </> show repo

    alreadyCloned <- doesDirectoryExist path
    if alreadyCloned
      then command_ [Cwd path] "git" ["fetch"]
      else do
        let url = "https://github.com/" <> show repo <> ".git"
        command_ [] "git" ["clone", "--recursive", url, path]

    return $ RunResult{runChanged = ChangedRecomputeDiff, runStore = "", runValue = path}
