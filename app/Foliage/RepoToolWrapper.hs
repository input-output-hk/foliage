module Foliage.RepoToolWrapper
  ( bootstrapRepo,
    createKeys,
    updateRepo,
  )
where

import Hackage.Security.RepoTool.Layout.Keys
import Hackage.Security.RepoTool.Main qualified as RepoTool
import Hackage.Security.RepoTool.Options
import Hackage.Security.RepoTool.Paths
import Hackage.Security.Server
import Hackage.Security.Util.Path

updateRepo :: FilePath -> FilePath -> IO ()
updateRepo keysDir repoDir = do
  keysLoc <- KeysLoc <$> makeAbsolute (fromFilePath keysDir)
  repoLoc <- RepoLoc <$> makeAbsolute (fromFilePath repoDir)
  RepoTool.bootstrapOrUpdate (globalOpts $ Update keysLoc repoLoc) keysLoc repoLoc True

bootstrapRepo :: FilePath -> FilePath -> IO ()
bootstrapRepo keysDir repoDir = do
  keysLoc <- KeysLoc <$> makeAbsolute (fromFilePath keysDir)
  repoLoc <- RepoLoc <$> makeAbsolute (fromFilePath repoDir)
  RepoTool.bootstrapOrUpdate (globalOpts $ Bootstrap keysLoc repoLoc) keysLoc repoLoc False

createKeys :: FilePath -> IO ()
createKeys keysDir = do
  keysLoc <- KeysLoc <$> makeAbsolute (fromFilePath keysDir)
  RepoTool.createKeys (globalOpts $ CreateKeys keysLoc) keysLoc

globalOpts :: Command -> GlobalOpts
globalOpts = GlobalOpts defaultKeysLayout hackageRepoLayout hackageIndexLayout True 1 10
