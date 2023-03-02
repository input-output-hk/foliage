{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.RemoteAsset
  ( fetchRemoteAsset,
    addFetchRemoteAssetRule,
  )
where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Network.URI (URI (..), URIAuth (..), pathSegments)
import Network.URI.Orphans ()
import System.Directory (createDirectoryIfMissing)

newtype RemoteAsset = RemoteAsset URI
  deriving (Show, Eq)
  deriving (Hashable, Binary, NFData) via URI

type instance RuleResult RemoteAsset = FilePath

fetchRemoteAsset :: URI -> Action FilePath
fetchRemoteAsset = apply1 . RemoteAsset

addFetchRemoteAssetRule :: FilePath -> Rules ()
addFetchRemoteAssetRule cacheDir = addBuiltinRule noLint noIdentity run
  where
    run :: BuiltinRun RemoteAsset FilePath
    run (RemoteAsset uri) old _mode = do
      unless (uriQuery uri == "") $
        fail $
          "Query elements in URI are not supported: " <> show uri

      unless (uriFragment uri == "") $
        fail $
          "Fragments in URI are not supported: " <> show uri

      let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
      host <- case uriRegName <$> uriAuthority uri of
        Nothing -> fail $ "invalid uri " ++ show uri
        Just host -> pure host
      let path = cacheDir </> joinPath (scheme : host : pathSegments uri)

      -- parse etag from store
      let oldETag = fromMaybe BS.empty old

      newETag <-
        withTempFile $ \fp -> traced "curl" $ do
          BS.writeFile fp oldETag
          createDirectoryIfMissing True (takeDirectory path)
          cmd_ Shell ["curl", "--silent", "--location", "--etag-compare", fp, "--etag-save", fp, "--output", path, show uri]
          BS.readFile fp

      let changed = if newETag == oldETag then ChangedRecomputeSame else ChangedRecomputeDiff
      return $ RunResult {runChanged = changed, runStore = newETag, runValue = path}
