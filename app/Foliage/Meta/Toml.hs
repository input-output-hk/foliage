module Foliage.Meta.Toml where

import Control.Category ((>>>))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16', encodeBase16)
import Data.Text (Text)
import Foliage.Time (UTCTime, utc, utcToZonedTime, zonedTimeToUTC)
import Toml (TomlCodec)
import Toml qualified

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

-- | Like 'Toml.Codec.BiMap.Conversion._ByteStringText' but uses base16 encoding
_ByteStringTextBase16 :: Toml.TomlBiMap ByteString Text
_ByteStringTextBase16 = Toml.invert $ Toml.prism encodeBase16 eitherByteString
  where
    eitherByteString :: Text -> Either Toml.TomlBiMapError ByteString
    eitherByteString = either (Left . Toml.ArbitraryError) Right . decodeBase16'

-- | Like 'Toml.Codec.BiMap.Conversion._ByteString' but uses base16 encoding
_ByteStringBase16 :: Toml.TomlBiMap ByteString Toml.AnyValue
_ByteStringBase16 = _ByteStringTextBase16 >>> Toml._Text

-- | Like 'Toml.Codec.Combinator.Primitive.byteString' but uses base16 encoding
byteStringBase16 :: Toml.Key -> TomlCodec ByteString
byteStringBase16 = Toml.match _ByteStringBase16
