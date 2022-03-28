{-# LANGUAGE FlexibleContexts #-}

module Foliage.HackageSecurity
  ( module Foliage.HackageSecurity,
    module Hackage.Security.Server,
    module Hackage.Security.TUF.FileMap,
    module Hackage.Security.Key.Env,
    module Hackage.Security.Util.Path,
    module Hackage.Security.Util.Some,
  )
where

import Control.Monad (replicateM_)
import Data.Functor.Identity
import Hackage.Security.Key.Env (fromKeys)
import Hackage.Security.Server
import Hackage.Security.TUF.FileMap
import Hackage.Security.Util.Path (fromFilePath, fromUnrootedFilePath, makeAbsolute, rootPath)
import Hackage.Security.Util.Some
import System.Directory (createDirectoryIfMissing)
import System.FilePath

readJSONSimple :: FromJSON ReadJSON_NoKeys_NoLayout a => FilePath -> IO (Either DeserializationError a)
readJSONSimple fp = do
  p <- makeAbsolute (fromFilePath fp)
  readJSON_NoKeys_NoLayout p

writeJSONSimple :: ToJSON Identity a => FilePath -> a -> IO ()
writeJSONSimple fp a = do
  p <- makeAbsolute (fromFilePath fp)
  writeJSON_NoLayout p a

computeFileInfoSimple :: FilePath -> IO FileInfo
computeFileInfoSimple fp = do
  p <- makeAbsolute (fromFilePath fp)
  computeFileInfo p

createKeys :: FilePath -> IO ()
createKeys base = do
  createDirectoryIfMissing True (base </> "root")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "root")
  createDirectoryIfMissing True (base </> "target")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "target")
  createDirectoryIfMissing True (base </> "timestamp")
  replicateM_ 1 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "timestamp")
  createDirectoryIfMissing True (base </> "snapshot")
  replicateM_ 1 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "snapshot")
  createDirectoryIfMissing True (base </> "mirrors")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "mirrors")

writeKeyWithId :: FilePath -> Some Key -> IO ()
writeKeyWithId base k =
  writeKey (base </> keyIdString (someKeyId k) <.> "json") k

writeKey :: FilePath -> Some Key -> IO ()
writeKey fp key = do
  p <- makeAbsolute (fromFilePath fp)
  writeJSON_NoLayout p key
