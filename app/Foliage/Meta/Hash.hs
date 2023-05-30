{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Foliage.Meta.Hash where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Development.Shake.Classes
import Foliage.Meta.Toml
import GHC.Generics (Generic)
import Toml qualified

newtype SHA256 = SHA256 {unSHA256 :: ByteString}
  deriving (Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

instance Show SHA256 where
  show (SHA256 bs) = show (T.unpack $ encodeBase64 bs)

instance ToJSON SHA256 where
  toJSON (SHA256 bs) = toJSON (encodeBase64 bs)

instance FromJSON SHA256 where
  parseJSON =
    parseJSON
      >=> either (parseFail . T.unpack) (pure . SHA256) . decodeBase64 . T.encodeUtf8

sha256Codec :: Toml.TomlCodec SHA256
sha256Codec = Toml.match (Toml.iso unSHA256 SHA256 >>> _ByteStringBase16) "sha256"

readFileHashValue :: FilePath -> IO SHA256
readFileHashValue = fmap (SHA256 . SHA256.hash) . BS.readFile
