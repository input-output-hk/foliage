{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.FetchURL (
  fetchURL,
  addFetchURLRule,
)
where

import Control.Exception (IOException, handle)
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
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Directory qualified as System.Directory
import System.Exit (ExitCode (..))
import System.IO (IOMode (ReadMode), withFile)

newtype FetchURL = FetchURL URI
  deriving (Eq)
  deriving (Hashable, Binary, NFData) via URI

instance Show FetchURL where
  show (FetchURL uri) = "fetchURL " ++ show uri

type instance RuleResult FetchURL = FilePath

fetchURL :: URI -> Action FilePath
fetchURL = apply1 . FetchURL

{- | Validate that a file is a valid gzip archive by checking magic bytes.
Returns False for non-existent files, too-small files, or invalid headers.

All gzip files start with magic bytes 0x1f 0x8b per RFC 1952 section 2.3.1.
This is a fast check (only reads 2 bytes) that catches corrupted/truncated files
before they cause tar extraction errors.
-}
validateGzipFile :: FilePath -> IO Bool
validateGzipFile path = handle (\(_ :: IOException) -> return False) $ do
  fileExists <- System.Directory.doesFileExist path
  if not fileExists
    then return False
    else withFile path ReadMode $ \h -> do
      header <- BS.hGet h 2
      -- gzip magic number: 0x1f 0x8b
      -- See: https://www.ietf.org/rfc/rfc1952.txt section 2.3.1
      return (BS.length header == 2 && header == BS.pack [0x1f, 0x8b])

addFetchURLRule :: FilePath -> Resource -> Int -> Rules ()
addFetchURLRule cacheDir downloadResource maxRetries = addBuiltinRule noLint noIdentity run
 where
  run :: BuiltinRun FetchURL FilePath
  run (FetchURL uri) old _mode = do
    unless (uriQuery uri == "") $
      error ("Query elements in URI are not supported: " <> show uri)

    unless (uriFragment uri == "") $
      error ("Fragments in URI are not supported: " <> show uri)

    let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri

    let host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)

    let path = cacheDir </> joinPath (scheme : host : pathSegments uri)

    -- parse etag from store
    let oldETag = fromMaybe BS.empty old

    -- Limit concurrent downloads to avoid overwhelming the server (e.g. GitHub 502s
    -- under high parallelism with -j 0). Non-network tasks still run at full parallelism.
    newETag <-
      withResource downloadResource 1 $
        withTempFile $ \etagFile -> do
          liftIO $ createDirectoryIfMissing True (takeDirectory path)
          liftIO $ BS.writeFile etagFile oldETag
          actionRetry 5 $ runCurl maxRetries uri path etagFile

    let changed = if newETag == oldETag then ChangedRecomputeSame else ChangedRecomputeDiff
    return $ RunResult{runChanged = changed, runStore = newETag, runValue = path}

runCurl :: Int -> URI -> String -> String -> Action ETag
runCurl maxRetries uri path etagFile = do
  -- Use atomic download pattern: download to temp file, validate, then move to cache.
  -- This prevents partial/corrupted downloads from persisting in the cache.
  -- Create temp file alongside the target path to ensure it's on the same filesystem
  -- (required for atomic rename)
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  let tmpPath = path ++ ".tmp"

  -- Ensure temp file doesn't exist from previous run
  liftIO $ do
    exists <- System.Directory.doesFileExist tmpPath
    when exists $ System.Directory.removeFile tmpPath

  (Exit exitCode, Stdout out) <-
    traced "curl" $ cmd Shell (curlInvocation tmpPath)
  case exitCode of
    ExitSuccess -> do
      -- Debug: check file size and first few bytes
      debugInfo <- liftIO $ do
        exists <- System.Directory.doesFileExist tmpPath
        if not exists
          then return "File doesn't exist"
          else do
            content <- BS.readFile tmpPath
            let size = BS.length content
            let firstBytes = BS.take 10 content
            return $ "Size: " ++ show size ++ " bytes, First bytes: " ++ show firstBytes

      -- Validate the downloaded file before moving to cache
      validGzip <- liftIO $ validateGzipFile tmpPath
      if validGzip
        then do
          -- Atomic move to cache location (all-or-nothing semantics)
          liftIO $ renameFile tmpPath path
          liftIO $ BS.readFile etagFile
        else do
          -- Clean up temp file before failing
          liftIO $ System.Directory.removeFile tmpPath
          error $
            unlines
              [ "Downloaded file failed gzip integrity check: " ++ show uri
              , debugInfo
              , "The file is corrupted or not a valid gzip archive."
              , "This may indicate a network error or server issue."
              ]
    ExitFailure c -> do
      -- Clean up temp file before failing
      liftIO $ do
        exists <- System.Directory.doesFileExist tmpPath
        when exists $ System.Directory.removeFile tmpPath
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
            error $ unlines ["calling", unwords (curlInvocation tmpPath), "failed with", errormsg]
 where
  curlInvocation tmpPath =
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
    , -- Retry on transient HTTP errors (408, 429, 500, 502, 503, 504)
      -- with exponential backoff
      "--retry"
    , show maxRetries
    , -- Also retry on connection refused (transient network issues)
      "--retry-connrefused"
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
      -- NOTE: Download to temporary file first, then atomically move to cache after validation
      "--output"
    , tmpPath
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
