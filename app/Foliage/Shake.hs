module Foliage.Shake
  ( computeFileInfoSimple',
    readKeysAt,
    readPackageVersionSpec',
  )
where

import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
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
