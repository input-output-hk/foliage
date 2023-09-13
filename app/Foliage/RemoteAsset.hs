{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.RemoteAsset (
  fetchRemoteAsset,
  addFetchRemoteAssetRule,
)
where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics (Generic)
import Network.URI (URI (..), URIAuth (..), pathSegments)
import Network.URI.Orphans ()
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))

newtype RemoteAsset = RemoteAsset URI
  deriving (Eq)
  deriving (Hashable, Binary, NFData) via URI

instance Show RemoteAsset where
  show (RemoteAsset uri) = "fetchRemoteAsset " ++ show uri

type instance RuleResult RemoteAsset = FilePath

fetchRemoteAsset :: URI -> Action FilePath
fetchRemoteAsset = apply1 . RemoteAsset

addFetchRemoteAssetRule :: FilePath -> Rules ()
addFetchRemoteAssetRule cacheDir = addBuiltinRule noLint noIdentity run
 where
  run :: BuiltinRun RemoteAsset FilePath
  run (RemoteAsset uri) old _mode = do
    unless (uriQuery uri == "") $
      error ("Query elements in URI are not supported: " <> show uri)

    unless (uriFragment uri == "") $
      error ("Fragments in URI are not supported: " <> show uri)

    let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri

    let host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)

    let path = cacheDir </> joinPath (scheme : host : pathSegments uri)

    -- parse etag from store
    let oldETag = fromMaybe BS.empty old

    newETag <-
      withTempFile $ \etagFile -> do
        liftIO $ createDirectoryIfMissing True (takeDirectory path)
        liftIO $ BS.writeFile etagFile oldETag
        actionRetry 5 $ runCurl uri path etagFile

    let changed = if newETag == oldETag then ChangedRecomputeSame else ChangedRecomputeDiff
    return $ RunResult{runChanged = changed, runStore = newETag, runValue = path}

runCurl :: URI -> String -> String -> Action ETag
runCurl uri path etagFile = do
  (Exit exitCode, Stdout out) <-
    traced "curl" $
      cmd
        Shell
        [ "curl"
        , -- Silent or quiet mode. Do not show progress meter or error messages. Makes Curl mute.
          "--silent"
        , -- Fail fast with no output at all on server errors.
          "--fail"
        , -- If the server reports that the requested page has moved to a different location this
          -- option will make curl redo the request on the new place.
          -- NOTE: This is needed because github always replies with a redirect
          "--location"
        , -- This  option  makes  a conditional HTTP request for the specific ETag read from the
          -- given file by sending a custom If-None-Match header using the stored ETag.
          -- For correct results, make sure that the specified file contains only a single line
          -- with the desired ETag. An empty file is parsed as an empty ETag.
          "--etag-compare"
        , etagFile
        , -- This option saves an HTTP ETag to the specified file. If no ETag is sent by the server,
          -- an empty file is created.
          "--etag-save"
        , etagFile
        , -- Write output to <file> instead of stdout.
          "--output"
        , path
        , "--write-out"
        , "%{json}"
        , -- URL to fetch
          show uri
        ]
  case exitCode of
    ExitSuccess -> liftIO $ BS.readFile etagFile
    ExitFailure c -> do
      -- We show the curl exit code only if we cannot parse curl's write-out.
      -- If we can parse it, we can craft a better error message.
      case Aeson.eitherDecode out :: Either String CurlWriteOut of
        Left err ->
          error $
            unlines
              [ "curl failed with return code " ++ show c ++ " while fetching " ++ show uri
              , "Error while reading curl diagnostic: " ++ err
              ]
        -- We can consider displaying different messages based on some fields (e.g. response_code)
        Right CurlWriteOut{errormsg} ->
          error errormsg

type ETag = BS.ByteString

-- Add what you need. See https://everything.curl.dev/usingcurl/verbose/writeout.
newtype CurlWriteOut = CurlWriteOut
  {errormsg :: String}
  deriving (Show, Generic)
  deriving anyclass (Aeson.FromJSON)
