{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.HackageSecurity where

import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.Traversable (for)

import Crypto.Sign.Ed25519 (unPublicKey)
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Development.Shake (
  Action,
  getDirectoryFiles,
  getShakeExtra,
  liftIO,
  need,
 )
import Hackage.Security.Server (
  FileInfo (..),
  Key,
  KeyId (..),
  KeyType (..),
  PublicKey (..),
  ToJSON,
  WriteJSON,
  computeFileInfo,
  createKey',
  hackageRepoLayout,
  readJSON_NoKeys_NoLayout,
  renderJSON,
  someKeyId,
  somePublicKey,
  withSignatures,
  writeJSON_NoLayout,
 )
import Hackage.Security.Util.Path qualified as Sec
import Hackage.Security.Util.Some (Some (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))

import Foliage.Options

computeFileInfoSimple :: FilePath -> Action FileInfo
computeFileInfoSimple path = do
  need [path]
  fi <- liftIO $ computeFileInfo (asRelativePath path)
  return $! forceFileInfo fi `seq` fi

forceFileInfo :: FileInfo -> ()
forceFileInfo (FileInfo a b) = a `seq` b `seq` ()

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
  p <- Sec.makeAbsolute (Sec.fromFilePath fp)
  writeJSON_NoLayout p key

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

readKeys :: FilePath -> Action [Some Key]
readKeys base = do
  Just signOptions <- getShakeExtra
  case signOptions of
    SignOptsSignWithKeys keysPath -> do
      paths <- getDirectoryFiles (keysPath </> base) ["*.json"]
      need $ map (\fn -> keysPath </> base </> fn) paths
      for paths $ \path -> do
        mKey <- liftIO $ readJSON_NoKeys_NoLayout (asRelativePath $ keysPath </> base </> path)
        case mKey of
          Left err -> fail $ show err
          Right key -> pure key
    SignOptsDon'tSign ->
      return []

renderSignedJSON :: (ToJSON WriteJSON a) => [Some Key] -> a -> BSL.ByteString
renderSignedJSON keys thing =
  renderJSON
    hackageRepoLayout
    (withSignatures hackageRepoLayout keys thing)

writeSignedJSON :: (ToJSON WriteJSON a) => FilePath -> [Some Key] -> a -> IO ()
writeSignedJSON path keys thing = do
  BSL.writeFile path $ renderSignedJSON keys thing

asRelativePath :: FilePath -> Sec.Path Sec.Relative
asRelativePath = Sec.rootPath @Sec.Relative . Sec.fromUnrootedFilePath
