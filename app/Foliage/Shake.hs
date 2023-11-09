module Foliage.Shake (
  readKeysAt,
)
where

import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Foliage.HackageSecurity

readKeysAt :: FilePath -> Action [Some Key]
readKeysAt base = do
  paths <- getDirectoryFiles base ["*.json"]
  need $ map (base </>) paths
  for paths $ \path -> do
    Right key <- liftIO $ readJSONSimple (base </> path)
    pure key
