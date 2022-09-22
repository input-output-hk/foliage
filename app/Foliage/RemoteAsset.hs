{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.RemoteAsset
  ( fetchRemoteAsset,
    addFetchRemoteAssetRule,
  )
where

import Data.ByteString qualified as BS
import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix qualified as Posix

type Url = String

newtype RemoteAsset = RemoteAsset Url
  deriving (Show, Eq)
  deriving (Hashable, Binary, NFData) via Url

type instance RuleResult RemoteAsset = FilePath

fetchRemoteAsset :: Url -> Action FilePath
fetchRemoteAsset = apply1 . RemoteAsset

addFetchRemoteAssetRule :: FilePath -> Rules ()
addFetchRemoteAssetRule cacheDir = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun RemoteAsset FilePath
    run (RemoteAsset url) old _mode = do
      let scheme : rest = Posix.splitPath url
          scheme' = dropWhileEnd (not . isAlpha) scheme
          path = cacheDir </> joinPath (scheme' : rest)

      -- parse etag from store
      let oldETag = fromMaybe BS.empty old

      newETag <-
        withTempFile $ \fp -> do
          liftIO $ BS.writeFile fp oldETag
          liftIO $ createDirectoryIfMissing True (takeDirectory path)
          cmd_ Shell ["curl", "--silent", "--location", "--etag-compare", fp, "--etag-save", fp, "--output", path, url]
          liftIO $ BS.readFile fp

      let changed = if newETag == oldETag then ChangedNothing else ChangedRecomputeDiff
      return $ RunResult {runChanged = changed, runStore = newETag, runValue = path}
