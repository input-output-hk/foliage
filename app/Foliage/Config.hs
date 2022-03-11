module Foliage.Config
  ( Config (..),
    Source (..),
    readConfig,
  )
where

import Data.List.NonEmpty as NE
import Data.Text (Text)
import Toml (TomlCodec, (.=)) -- add 'TomlBiMap' and 'Key' here optionally
import Toml qualified

newtype Config = Config
  { configSources :: [Source]
  }
  deriving (Show)

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.list sourceCodec "sources" .= configSources

data Source = Source
  { sourceUrl :: Text,
    sourceSubdirs :: [Text]
  }
  deriving (Show)

sourceCodec :: TomlCodec Source
sourceCodec =
  Source
    <$> Toml.text "url" .= sourceUrl
    <*> subdirsCodec .= sourceSubdirs

subdirsCodec :: TomlCodec [Text]
subdirsCodec =
  Toml.dimap
    NE.nonEmpty
    (maybe [] toList)
    (Toml.dioptional $ Toml.arrayNonEmptyOf Toml._Text "subdirs")

readConfig :: FilePath -> IO (Either String Config)
readConfig fp = do
  tomlRes <- Toml.decodeFileEither configCodec fp
  return $ case tomlRes of
    Left e -> Left (show e)
    Right settings -> Right settings
