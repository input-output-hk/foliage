{-# LANGUAGE FlexibleContexts #-}

module Foliage.Shake
  ( computeFileInfoSimple',
    readKeysAt,
    readPackageVersionMeta',
    writeSignedJSON,
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

readPackageVersionMeta' :: FilePath -> Action PackageVersionMeta
readPackageVersionMeta' fp = do
  need [fp]
  liftIO $ readPackageVersionMeta fp

writeSignedJSON :: ToJSON WriteJSON a => Path Absolute -> (RepoLayout -> RepoPath) -> [Some Key] -> a -> Action ()
writeSignedJSON outputDirRoot repoPath keys thing = do
  putInfo $ "Writing " ++ show (repoPath hackageRepoLayout)
  liftIO $ writeLazyByteString fp $ renderSignedJSON keys thing
  where
    fp = anchorRepoPathLocally outputDirRoot $ repoPath hackageRepoLayout
