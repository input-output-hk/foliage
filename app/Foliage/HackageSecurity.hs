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

import Control.Monad (replicateM)
import Crypto.Sign.Ed25519 (unPublicKey)
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Text qualified as T
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

forceFileInfo :: FileInfo -> ()
forceFileInfo (FileInfo a b) = a `seq` b `seq` ()

computeFileInfoSimple :: FilePath -> IO FileInfo
computeFileInfoSimple fp = do
  p <- makeAbsolute (fromFilePath fp)
  fi <- computeFileInfo p
  return $! forceFileInfo fi `seq` fi

createKeys :: FilePath -> IO ()
createKeys base = do
  putStrLn "root keys:"
  createKeyGroup "root" >>= showKeys
  for_ ["target", "timestamp", "snapshot", "mirrors"] createKeyGroup
  where
    createKeyGroup group = do
      createDirectoryIfMissing True (base </> group)
      keys <- replicateM 3 $ createKey' KeyTypeEd25519
      for_ keys $ writeKeyWithId (base </> group)
      pure keys

    showKeys keys =
      for_ keys $ \key ->
        putStrLn $ "  " ++ showKey key

showKey :: Some Key -> [Char]
showKey k = T.unpack $ encodeBase16 $ exportSomePublicKey $ somePublicKey k

writeKeyWithId :: FilePath -> Some Key -> IO ()
writeKeyWithId base k =
  writeKey (base </> keyIdString (someKeyId k) <.> "json") k

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
