{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

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
import Crypto.Sign.Ed25519 (unPublicKey)
import Data.ByteString.Base16 qualified as Base16
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy qualified as BSL
import Hackage.Security.Key.Env (fromKeys)
import Hackage.Security.Server
import Hackage.Security.TUF.FileMap
import Hackage.Security.Util.Path (Absolute, Path, fromFilePath, fromUnrootedFilePath, makeAbsolute, rootPath, writeLazyByteString)
import Hackage.Security.Util.Some
import System.Directory (createDirectoryIfMissing)
import System.FilePath

readJSONSimple :: FromJSON ReadJSON_NoKeys_NoLayout a => FilePath -> IO (Either DeserializationError a)
readJSONSimple fp = do
  p <- makeAbsolute (fromFilePath fp)
  readJSON_NoKeys_NoLayout p

computeFileInfoSimple :: FilePath -> IO FileInfo
computeFileInfoSimple fp = do
  p <- makeAbsolute (fromFilePath fp)
  computeFileInfo p

createKeys :: FilePath -> IO ()
createKeys base = do
  putStrLn "  root keys:"
  createDirectoryIfMissing True (base </> "root")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "root")
  putStrLn "  target keys:"
  createDirectoryIfMissing True (base </> "target")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "target")
  putStrLn "  timestamp keys:"
  createDirectoryIfMissing True (base </> "timestamp")
  replicateM_ 1 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "timestamp")
  putStrLn "  snapshot keys:"
  createDirectoryIfMissing True (base </> "snapshot")
  replicateM_ 1 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "snapshot")
  putStrLn "  mirrors keys:"
  createDirectoryIfMissing True (base </> "mirrors")
  replicateM_ 3 $ createKey' KeyTypeEd25519 >>= writeKeyWithId (base </> "mirrors")

writeKeyWithId :: FilePath -> Some Key -> IO ()
writeKeyWithId base k = do
  let keyId' = keyIdString $ someKeyId k
  let publicKey' = somePublicKey k
  putStr "    "
  putStrLn $ BS.unpack $ Base16.encode $ exportSomePublicKey publicKey'

  writeKey (base </> keyId' <.> "json") k

exportSomePublicKey :: Some PublicKey -> BS.ByteString
exportSomePublicKey (Some k) = exportPublicKey k

exportPublicKey :: PublicKey a -> BS.ByteString
exportPublicKey (PublicKeyEd25519 pub) = unPublicKey pub

writeKey :: FilePath -> Some Key -> IO ()
writeKey fp key = do
  p <- makeAbsolute (fromFilePath fp)
  writeJSON_NoLayout p key

renderSignedJSON :: ToJSON WriteJSON a => [Some Key] -> a -> BSL.ByteString
renderSignedJSON keys thing =
  renderJSON
    hackageRepoLayout
    (withSignatures hackageRepoLayout keys thing)

writeSignedJSON :: ToJSON WriteJSON a => Path Absolute -> (RepoLayout -> RepoPath) -> [Some Key] -> a -> IO ()
writeSignedJSON outputDirRoot repoPath keys thing = do
  writeLazyByteString fp $ renderSignedJSON keys thing
  where
    fp = anchorRepoPathLocally outputDirRoot $ repoPath hackageRepoLayout
