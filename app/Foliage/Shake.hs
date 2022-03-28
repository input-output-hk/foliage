module Foliage.Shake
  ( computeFileInfoSimple',
    readFileByteStringLazy,
    readKeysAt,
    readSourceMeta',
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Foliage.HackageSecurity
import Foliage.Meta

computeFileInfoSimple' :: FilePath -> Action FileInfo
computeFileInfoSimple' fp = do
  need [fp] >> liftIO (computeFileInfoSimple fp)

readFileByteStringLazy :: FilePath -> Action BSL.ByteString
readFileByteStringLazy x = need [x] >> liftIO (BSL.readFile x)

readKeysAt :: FilePath -> Action [Some Key]
readKeysAt base = do
  paths <- getDirectoryFiles base ["*.private"]
  need $ map (base </>) paths
  for paths $ \path -> do
    Right key <- liftIO $ readJSONSimple (base </> path)
    pure key

readSourceMeta' :: FilePath -> Action SourceMeta
readSourceMeta' fp = do
  need [fp]
  liftIO $ readSourceMeta fp
