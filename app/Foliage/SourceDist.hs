{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.SourceDist (
  extractFromTarball,
  cachePathForURL,
  fetchPackageVersion,
  applyPatches,
  updateCabalFileVersion,
)
where

import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)

import Development.Shake (
  Action,
  CmdOption (..),
  cmd_,
  getDirectoryFiles,
  liftIO,
  putInfo,
  withTempDir,
  (<//>),
 )
import Development.Shake.FilePath (
  replaceBaseName,
  takeDirectory,
  (</>),
 )
import Distribution.Compat.Lens (set)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple (Version)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Lens qualified as L
import Distribution.Verbosity qualified as Verbosity
import Network.URI (URI (..), URIAuth (..))
import System.Directory qualified as IO

import Foliage.FetchURL (fetchURL)
import Foliage.Meta (PackageVersionSource (..))
import Foliage.Utils.GitHub (githubRepoTarballUrl)

applyPatches :: FilePath -> FilePath -> Action ()
applyPatches metaFile pkgDir = do
  patchfiles <- getDirectoryFiles (takeDirectory metaFile) ["patches/*.patch"]
  for_ patchfiles $ \patchfile -> do
    -- _sources/name/version/meta.toml -> _sources/name/version/patches/some.patch
    let patch = replaceBaseName metaFile patchfile
    -- FileStdin is relative to the current working directory of this process
    -- but patch needs to be run in srcDir
    cmd_ Shell (Cwd pkgDir) (FileStdin patch) "patch -p1"

fetchPackageVersion :: FilePath -> PackageVersionSource -> FilePath -> Action ()
fetchPackageVersion _cacheDir (URISource (URI{uriScheme, uriPath}) mSubdir) pkgDir | uriScheme == "file:" = do
  tarballPath <- liftIO $ IO.makeAbsolute uriPath
  extractFromTarball tarballPath mSubdir pkgDir
fetchPackageVersion cacheDir (URISource uri mSubdir) pkgDir = do
  tarballPath <- cachePathForURL cacheDir uri
  fetchURL uri tarballPath
  extractFromTarball tarballPath mSubdir pkgDir
fetchPackageVersion cacheDir (GitHubSource repo rev mSubdir) pkgDir = do
  let uri = githubRepoTarballUrl repo rev
  tarballPath <- cachePathForURL cacheDir uri
  fetchURL uri tarballPath
  extractFromTarball tarballPath mSubdir pkgDir

updateCabalFileVersion :: FilePath -> Version -> Action ()
updateCabalFileVersion path pkgVersion = do
  pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.normal path
  let pkgDesc' = set (L.packageDescription . L.package . L.pkgVersion) pkgVersion pkgDesc
  putInfo $ "Updating version in cabal file" ++ path
  liftIO $ writeGenericPackageDescription path pkgDesc'

cachePathForURL :: FilePath -> URI -> Action FilePath
cachePathForURL cacheDir uri = do
  let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
      host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)
  return $ cacheDir </> scheme </> host <//> uriPath uri

extractFromTarball :: FilePath -> Maybe FilePath -> FilePath -> Action ()
extractFromTarball tarballPath mSubdir destDir = do
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
        destDir
      ]
