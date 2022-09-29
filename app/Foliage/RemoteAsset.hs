{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.RemoteAsset
  ( remoteAssetNeed,
    remoteAssetRule,
    addBuiltinRemoteAssetRule,
  )
where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Network.URI (URI (uriAuthority, uriFragment, uriQuery, uriScheme), URIAuth (uriRegName), pathSegments)
import Network.URI.Orphans ()
import System.Directory (createDirectoryIfMissing)

newtype RemoteAsset = RemoteAsset URI
  deriving (Show, Eq)
  deriving (Hashable, Binary, NFData) via URI

type instance RuleResult RemoteAsset = FilePath

data RemoteAssetRule = RemoteAssetRule RemoteAsset (Action FilePath)

remoteAssetRule :: URI -> Action FilePath -> Rules ()
remoteAssetRule url act = addUserRule $ RemoteAssetRule (RemoteAsset url) act

remoteAssetNeed :: URI -> Action FilePath
remoteAssetNeed = apply1 . RemoteAsset

addBuiltinRemoteAssetRule :: FilePath -> Rules ()
addBuiltinRemoteAssetRule cacheDir = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun RemoteAsset FilePath
    run (RemoteAsset uri) old _mode = do
      unless (uriQuery uri == "") $
        fail $ "Query elements in URI are not supported: " <> show uri

      unless (uriFragment uri == "") $
        fail $ "Fragments in URI are not supported: " <> show uri

      let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
          Just host = uriRegName <$> uriAuthority uri
          path = cacheDir </> joinPath (scheme : host : pathSegments uri)

      -- parse etag from store
      let oldETag = fromMaybe BS.empty old

      newETag <-
        withTempFile $ \fp -> do
          liftIO $ BS.writeFile fp oldETag
          liftIO $ createDirectoryIfMissing True (takeDirectory path)
          cmd_ Shell ["curl", "--silent", "--location", "--etag-compare", fp, "--etag-save", fp, "--output", path, show uri]
          liftIO $ BS.readFile fp

      let changed = if newETag == oldETag then ChangedNothing else ChangedRecomputeDiff
      return $ RunResult {runChanged = changed, runStore = newETag, runValue = path}
