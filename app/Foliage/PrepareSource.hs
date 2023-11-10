{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSource where

import Data.Char (isAlpha)
import Data.List (dropWhileEnd)
import Development.Shake
import Foliage.Oracles (CacheDir (..))
import Network.URI (URI (..), URIAuth (..), pathSegments)
import System.Directory qualified as IO
import System.FilePath (joinPath, (</>))

extractFromTarball :: FilePath -> Maybe FilePath -> FilePath -> Action ()
extractFromTarball tarballPath mSubdir outDir = do
  withTempDir $ \tmpDir -> do
    cmd_
      [ "tar"
      , -- Extract files from an archive
        "--extract"
      , -- Filter the archive through gunzip
        "--gunzip"
      , -- Recursively remove all files in the directory prior to extracting it
        "--recursive-unlink"
      , -- Use archive file
        "--file"
      , tarballPath
      , -- Change to DIR before performing any operations
        "--directory"
      , tmpDir
      ]

    ls <-
      -- remove "." and ".."
      filter (not . all (== '.'))
        -- NOTE: Don't let shake look into tmpDir! it will cause
        -- unnecessary rework because tmpDir is always new
        <$> liftIO (IO.getDirectoryContents tmpDir)

    -- Special treatment of top-level directory: we remove it
    let byPassSingleTopLevelDir = case ls of [l] -> (</> l); _ -> id
        applyMSubdir = case mSubdir of Just s -> (</> s); _ -> id
        srcDir = applyMSubdir $ byPassSingleTopLevelDir tmpDir

    cmd_
      [ "cp"
      , -- copy directories recursively
        "--recursive"
      , -- treat DEST as a normal file
        "--no-target-directory"
      , -- always follow symbolic links in SOURCE
        "--dereference"
      , -- SOURCE
        srcDir
      , -- DEST
        outDir
      ]

cachePathForURL :: URI -> Action FilePath
cachePathForURL uri = do
  let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
  let host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)
  cacheDir <- askOracle CacheDir
  let path = cacheDir </> joinPath (scheme : host : pathSegments uri)
  return path
