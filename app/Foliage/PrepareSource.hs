{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSource where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text qualified as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId
import Distribution.Types.PackageName (unPackageName)
import Foliage.Meta
import Foliage.RemoteAsset (fetchRemoteAsset)
import Foliage.UpdateCabalFile (rewritePackageVersion)
import GHC.Generics
import Network.URI (URI (..), URIAuth (..), nullURI, nullURIAuth)
import System.Directory qualified as IO
import System.FilePath ((<.>), (</>))

data PrepareSourceRule = PrepareSourceRule PackageId PackageVersionSpec
  deriving (Show, Eq, Generic)
  deriving (Hashable, Binary, NFData)

type instance RuleResult PrepareSourceRule = FilePath

prepareSource :: PackageId -> PackageVersionSpec -> Action FilePath
prepareSource pkgId pkgMeta = apply1 $ PrepareSourceRule pkgId pkgMeta

addPrepareSourceRule :: FilePath -> FilePath -> Rules ()
addPrepareSourceRule inputDir cacheDir = addBuiltinRule noLint noIdentity run
  where
    run :: PrepareSourceRule -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
    run (PrepareSourceRule pkgId pkgMeta) _old mode = do
      let PackageIdentifier {pkgName, pkgVersion} = pkgId
      let PackageVersionSpec {packageVersionSource, packageVersionForce} = pkgMeta
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
            TarballSource url mSubdir -> do
              tarballPath <- fetchRemoteAsset url

              withTempDir $ \tmpDir -> do
                cmd_ "tar xzf" [tarballPath] "-C" [tmpDir]

                -- Special treatment of top-level directory: we remove it
                --
                -- Note: Don't let shake look into tmpDir! it will cause
                -- unnecessary rework because tmpDir is always new
                ls <- liftIO $ IO.getDirectoryContents tmpDir
                let ls' = filter (not . all (== '.')) ls

                let fix1 = case ls' of [l] -> (</> l); _ -> id
                    fix2 = case mSubdir of Just s -> (</> s); _ -> id
                    tdir = fix2 $ fix1 tmpDir

                cmd_ "cp --recursive --no-target-directory --dereference" [tdir, srcDir]
            --
            -- This is almost identical to the above but we get to keep the
            -- metadata.
            --
            GitHubSource repo rev mSubdir -> do
              let url =
                    nullURI
                      { uriScheme = "https:",
                        uriAuthority = Just nullURIAuth {uriRegName = "github.com"},
                        uriPath = "/" </> T.unpack (unGitHubRepo repo) </> "tarball" </> T.unpack (unGitHubRev rev)
                      }

              tarballPath <- fetchRemoteAsset url

              withTempDir $ \tmpDir -> do
                cmd_ "tar xzf" [tarballPath] "-C" [tmpDir]

                -- Special treatment of top-level directory: we remove it
                --
                -- Note: Don't let shake look into tmpDir! it will cause
                -- unnecessary rework because tmpDir is always new
                ls <- liftIO $ IO.getDirectoryContents tmpDir
                let ls' = filter (not . all (== '.')) ls

                let fix1 = case ls' of [l] -> (</> l); _ -> id
                    fix2 = case mSubdir of Just s -> (</> s); _ -> id
                    tdir = fix2 $ fix1 tmpDir

                cmd_ "cp --recursive --no-target-directory --dereference" [tdir, srcDir]

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
