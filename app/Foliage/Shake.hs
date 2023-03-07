module Foliage.Shake
  ( computeFileInfoSimple',
    readKeysAt,
    readPackageVersionSpec',
    readGenericPackageDescription',
  )
where

import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Simple.PackageDescription
import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta

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
