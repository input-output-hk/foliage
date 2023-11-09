{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSource where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId
import Distribution.Types.PackageName (unPackageName)
import Foliage.FetchURL (fetchURL)
import Foliage.Meta
import Foliage.Oracles (CacheDir (..), InputDir (..))
import Foliage.UpdateCabalFile (rewritePackageVersion)
import Foliage.Utils.GitHub (githubRepoTarballUrl)
import GHC.Generics
import Network.URI (URI (..), URIAuth (..), pathSegments)
import System.Directory qualified as IO
import System.FilePath (joinPath, (<.>), (</>))

data PrepareSourceRule = PrepareSourceRule PackageId PackageVersionSpec
  deriving (Eq, Generic)
  deriving (Hashable, Binary, NFData)

instance Show PrepareSourceRule where
  show (PrepareSourceRule pkgId pkgSpec) =
    "prepareSource "
      ++ prettyShow pkgId
      ++ " "
      ++ show pkgSpec

type instance RuleResult PrepareSourceRule = FilePath

prepareSource :: PackageId -> PackageVersionSpec -> Action FilePath
prepareSource pkgId pkgMeta = apply1 $ PrepareSourceRule pkgId pkgMeta

addPrepareSourceRule :: Rules ()
addPrepareSourceRule = addBuiltinRule noLint noIdentity run
 where
  run :: PrepareSourceRule -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
  run (PrepareSourceRule pkgId pkgMeta) _old mode = do
    let PackageIdentifier{pkgName, pkgVersion} = pkgId
    let PackageVersionSpec{packageVersionSource, packageVersionForce} = pkgMeta

    cacheDir <- askOracle CacheDir
    let srcDir = cacheDir </> unPackageName pkgName </> prettyShow pkgVersion

    case mode of
      RunDependenciesSame ->
        return $ RunResult ChangedNothing BS.empty srcDir
      RunDependenciesChanged -> do
        -- FIXME too much rework?
        -- this action only depends on the tarball and the package metadata

        -- delete everything inside the package source tree
        liftIO $ do
          -- FIXME this should only delete inside srcDir but apparently
          -- also deletes srcDir itself
          removeFiles srcDir ["//*"]
          IO.createDirectoryIfMissing True srcDir

        case packageVersionSource of
          URISource (URI{uriScheme, uriPath}) mSubdir | uriScheme == "file:" -> do
            tarballPath <- liftIO $ IO.makeAbsolute uriPath
            extractFromTarball tarballPath mSubdir srcDir
          URISource uri mSubdir -> do
            tarballPath <- cachePathForURL uri
            fetchURL uri tarballPath
            extractFromTarball tarballPath mSubdir srcDir
          GitHubSource repo rev mSubdir -> do
            let uri = githubRepoTarballUrl repo rev
            tarballPath <- cachePathForURL uri
            fetchURL uri tarballPath
            extractFromTarball tarballPath mSubdir srcDir

        inputDir <- askOracle InputDir
        let patchesDir = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "patches"
        hasPatches <- doesDirectoryExist patchesDir

        when hasPatches $ do
          patchfiles <- getDirectoryFiles patchesDir ["*.patch"]
          for_ patchfiles $ \patchfile -> do
            let patch = patchesDir </> patchfile
            cmd_ Shell (Cwd srcDir) (FileStdin patch) "patch -p1"

        when packageVersionForce $ do
          let cabalFilePath = srcDir </> unPackageName pkgName <.> "cabal"
          putInfo $ "Updating version in cabal file" ++ cabalFilePath
          liftIO $ rewritePackageVersion cabalFilePath pkgVersion

        return $ RunResult ChangedRecomputeDiff BS.empty srcDir

  extractFromTarball tarballPath mSubdir outDir = do
    withTempDir $ \tmpDir -> do
      cmd_
        [ "tar"
        , -- Extract files from an archive
          "--extract"
        , -- Filter the archive through gunzip
          "--gunzip"
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
