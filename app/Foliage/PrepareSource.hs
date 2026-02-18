{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSource where

import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId
import Distribution.Types.PackageName (unPackageName)
import Foliage.FetchURL (fetchURL)
import Foliage.Meta
import Foliage.Utils.GitHub (githubRepoTarballUrl)
import GHC.Generics
import Network.URI (URI (..))
import System.Directory qualified as IO
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((<.>), (</>))

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

addPrepareSourceRule :: FilePath -> FilePath -> Rules ()
addPrepareSourceRule inputDir cacheDir = addBuiltinRule noLint noIdentity run
 where
  run :: PrepareSourceRule -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
  run (PrepareSourceRule pkgId pkgMeta) _old mode = do
    let PackageIdentifier{pkgName, pkgVersion} = pkgId
    let PackageVersionSpec{packageVersionSource, packageVersionForce} = pkgMeta
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
            -- Pass False to indicate this is NOT a cached download
            extractFromTarball tarballPath mSubdir srcDir False
          URISource uri mSubdir -> do
            tarballPath <- fetchURL uri
            -- Pass True to indicate this IS a cached download
            extractFromTarball tarballPath mSubdir srcDir True
          GitHubSource repo rev mSubdir -> do
            tarballPath <- fetchURL (githubRepoTarballUrl repo rev)
            -- Pass True to indicate this IS a cached download
            extractFromTarball tarballPath mSubdir srcDir True

        let patchesDir = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "patches"
        hasPatches <- doesDirectoryExist patchesDir

        when hasPatches $ do
          patchfiles <- getDirectoryFiles patchesDir ["*.patch"]
          for_ patchfiles $ \patchfile -> do
            let patch = patchesDir </> patchfile
            cmd_ Shell (Cwd srcDir) (FileStdin patch) "patch -p1"

        when packageVersionForce $ do
          let revisionZero = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "revisions" </> "0" <.> "cabal"
          hasRevisionZero <- doesFileExist revisionZero
          unless hasRevisionZero $
            error $
              "force-version requires a modified Cabal file in " ++ revisionZero
          let cabalFilePath = srcDir </> unPackageName pkgName <.> "cabal"
          putInfo $ "Copying revision 0 of cabal file to " ++ cabalFilePath
          copyFileChanged revisionZero cabalFilePath

        return $ RunResult ChangedRecomputeDiff BS.empty srcDir

  extractFromTarball tarballPath mSubdir outDir isFromCache = do
    withTempDir $ \tmpDir -> do
      -- Use cmd instead of cmd_ to capture exit code and handle errors properly
      Exit exitCode <-
        cmd
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

      case exitCode of
        ExitSuccess -> do
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
            , -- SOURCE
              srcDir
            , -- DEST
              outDir
            ]
        ExitFailure code -> do
          -- Only delete cached files, not user-provided file: sources
          when isFromCache $ do
            liftIO $ IO.removePathForcibly tarballPath

          error $
            unlines
              [ "Tar extraction failed with exit code " ++ show code
              , "Tarball: " ++ tarballPath
              , if isFromCache
                  then "The corrupted cache file has been removed. Please run the build again to re-download the file."
                  else "User-provided tarball may be corrupted."
              ]
