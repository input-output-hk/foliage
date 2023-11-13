{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist (
  -- prepareSdist,
  prepareSource,
)
where

import Control.Monad (when)
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Compat.Lens (set)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Lens qualified as L
import Distribution.Verbosity qualified as Verbosity
import Foliage.FetchURL (fetchURL)
import Foliage.Meta (PackageVersionSource (..), PackageVersionSpec (..))
import Foliage.Oracles (CacheDir (..))
import Foliage.Utils.GitHub (githubRepoTarballUrl)
import Network.URI (URI (..), URIAuth (..))
import System.Directory qualified as IO

prepareSource :: FilePath -> PackageIdentifier -> PackageVersionSpec -> FilePath -> Action ()
prepareSource metaFile pkgId pkgSpec cacheDir = do
  let PackageIdentifier{pkgName, pkgVersion} = pkgId
      PackageVersionSpec{packageVersionSource, packageVersionForce} = pkgSpec

  cacheRoot <- askOracle CacheDir
  let cachePathForURL uri =
        let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
            host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)
         in cacheRoot </> scheme </> host <//> uriPath uri

  case packageVersionSource of
    URISource (URI{uriScheme, uriPath}) mSubdir | uriScheme == "file:" -> do
      tarballPath <- liftIO $ IO.makeAbsolute uriPath
      extractFromTarball tarballPath mSubdir cacheDir
    URISource uri mSubdir -> do
      let tarballPath = cachePathForURL uri
      fetchURL uri tarballPath
      extractFromTarball tarballPath mSubdir cacheDir
    GitHubSource repo rev mSubdir -> do
      let uri = githubRepoTarballUrl repo rev
          tarballPath = cachePathForURL uri
      fetchURL uri tarballPath
      extractFromTarball tarballPath mSubdir cacheDir

  patchfiles <- getDirectoryFiles (takeDirectory metaFile) ["patches/*.patch"]
  for_ patchfiles $ \patchfile -> do
    -- _sources/name/version/meta.toml -> _sources/name/version/patches/some.patch
    let patch = replaceBaseName metaFile patchfile
    -- FileStdin is relative to the current working directory of this process
    -- but patch needs to be run in srcDir
    cmd_ Shell (Cwd cacheDir) (FileStdin patch) "patch -p1"

  let cabalFilePath = cacheDir </> unPackageName pkgName <.> "cabal"
  when packageVersionForce $ do
    pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.normal cabalFilePath
    let pkgDesc' = set (L.packageDescription . L.package . L.pkgVersion) pkgVersion pkgDesc
    putInfo $ "Updating version in cabal file" ++ cabalFilePath
    liftIO $ writeGenericPackageDescription cabalFilePath pkgDesc'

-- liftIO $ packageDirToSdist Verbosity.normal pkgDesc (takeDirectory cabalFilePath) >>= BSL.writeFile path

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
