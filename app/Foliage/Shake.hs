module Foliage.Shake
  ( computeFileInfoSimple',
    readKeysAt,
    readPackageVersionSpec',
    readGenericPackageDescription',
    originalCabalFile,
  )
where

import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Simple.PackageDescription
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta
import Foliage.PrepareSource

computeFileInfoSimple' :: FilePath -> Action FileInfo
computeFileInfoSimple' fp = do
  need [fp]
  liftIO $ computeFileInfoSimple fp

readKeysAt :: FilePath -> Action [Some Key]
readKeysAt base = do
  paths <- getDirectoryFiles base ["*.json"]
  need $ map (base </>) paths
  for paths $ \path -> do
    Right key <- liftIO $ readJSONSimple (base </> path)
    pure key

readPackageVersionSpec' :: FilePath -> Action PackageVersionSpec
readPackageVersionSpec' fp = do
  need [fp]
  liftIO $ readPackageVersionSpec fp

readGenericPackageDescription' :: FilePath -> Action GenericPackageDescription
readGenericPackageDescription' fp = do
  need [fp]
  liftIO $ readGenericPackageDescription Verbosity.silent fp

originalCabalFile :: PackageId -> PackageVersionSpec -> Action FilePath
originalCabalFile pkgId pkgSpec = do
  srcDir <- prepareSource pkgId pkgSpec
  return $ srcDir </> unPackageName (pkgName pkgId) <.> "cabal"
