{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.FetchURL (
  fetchURL,
  addFetchURLRule,
)
where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics (Generic)
import Network.URI (URI (..))
import Network.URI.Orphans ()
import System.Directory (createDirectoryIfMissing)
import System.Directory qualified as IO
import System.Exit (ExitCode (..))

data FetchURL = FetchURL URI FilePath
  deriving (Generic, Eq)
  deriving anyclass (Hashable, Binary, NFData)

instance Show FetchURL where
  show (FetchURL uri _path) = "fetchURL " ++ show uri

type instance RuleResult FetchURL = ()

fetchURL :: URI -> FilePath -> Action ()
fetchURL uri path = do
  produces [path]
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory path)
  withTempFile $ \etagFile ->
    void $ actionRetry 5 $ runCurl uri path etagFile

-- apply1 (FetchURL uri path)

addFetchURLRule :: Rules ()
addFetchURLRule = addBuiltinRule noLint noIdentity run
 where
  run (FetchURL uri path) oldETag _mode = do
    -- For safety promise shake that we are going to produce that file
    produces [path]

    unless (uriQuery uri == "") $
      error ("Query elements in URI are not supported: " <> show uri)

    unless (uriFragment uri == "") $
      error ("Fragments in URI are not supported: " <> show uri)

    newETag <-
      withTempFile $ \etagFile -> do
        liftIO $ do
          createDirectoryIfMissing True (takeDirectory path)
          -- We write the etag file only if we have already have both an etag and a downloaded file. Otherwise the file could be missing and curl will still not redownload it.
          fe <- IO.doesFileExist path
          when fe $
            case oldETag of
              Just tag | fe -> BS.writeFile etagFile tag
              _otherwise -> pure ()
        actionRetry 5 $ runCurl uri path etagFile

    let changed = if Just newETag == oldETag then ChangedRecomputeSame else ChangedRecomputeDiff
    return RunResult{runChanged = changed, runStore = newETag, runValue = ()}

runCurl :: URI -> String -> String -> Action ETag
runCurl uri path etagFile =
  cmd Shell curlInvocation >>= \case
    (Exit ExitSuccess, Stdout _out) ->
      liftIO $ BS.readFile etagFile
    (Exit (ExitFailure c), Stdout out) -> do
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
          error $ unlines ["calling", unwords curlInvocation, "failed with", errormsg]
 where
  curlInvocation =
    [ "curl"
    , -- Silent or quiet mode. Do not show progress meter or error messages. Makes Curl mute.
      "--silent"
    , -- ... but still show errors
      "--show-error"
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

type ETag = BS.ByteString

-- Add what you need. See https://everything.curl.dev/usingcurl/verbose/writeout.
newtype CurlWriteOut = CurlWriteOut
  {errormsg :: String}
  deriving (Show, Generic)
  deriving anyclass (Aeson.FromJSON)
