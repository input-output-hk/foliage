{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Foliage.Meta
  ( SourceMeta,
    pattern SourceMeta,
    sourceTimestamp,
    sourceUrl,
    sourceSubdir,
    sourceRevisions,
    readSourceMeta,
    writeSourceMeta,
    RevisionMeta,
    pattern RevisionMeta,
    revisionTimestamp,
    revisionNumber,
    UTCTime,
    latestRevisionNumber,
  )
where

import Control.Monad (void)
import Data.Time.Format.ISO8601
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Development.Shake.Classes
import Foliage.Time
import GHC.Generics
import Toml (TomlCodec, (.=))
import Toml qualified

data SourceMeta = SourceMeta' WrapUTCTime String (Maybe String) [RevisionMeta]
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern SourceMeta :: UTCTime -> String -> Maybe String -> [RevisionMeta] -> SourceMeta
pattern SourceMeta {sourceTimestamp, sourceUrl, sourceSubdir, sourceRevisions} =
  SourceMeta' (WrapUTCTime sourceTimestamp) sourceUrl sourceSubdir sourceRevisions

sourceMetaCodec :: TomlCodec SourceMeta
sourceMetaCodec =
  SourceMeta
    <$> timeCodec "timestamp" .= sourceTimestamp
    <*> Toml.string "url" .= sourceUrl
    <*> Toml.dioptional (Toml.string "subdir") .= sourceSubdir
    <*> Toml.list revisionMetaCodec "revisions" .= sourceRevisions

readSourceMeta :: FilePath -> IO SourceMeta
readSourceMeta = Toml.decodeFile sourceMetaCodec

writeSourceMeta :: FilePath -> SourceMeta -> IO ()
writeSourceMeta fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionMeta = RevisionMeta' WrapUTCTime Int
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern RevisionMeta :: UTCTime -> Int -> RevisionMeta
pattern RevisionMeta {revisionTimestamp, revisionNumber} =
  RevisionMeta' (WrapUTCTime revisionTimestamp) revisionNumber

revisionMetaCodec :: TomlCodec RevisionMeta
revisionMetaCodec =
  RevisionMeta
    <$> timeCodec "timestamp" .= revisionTimestamp
    <*> Toml.int "number" .= revisionNumber

newtype WrapUTCTime = WrapUTCTime {unwrapUTCTime :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (ISO8601) via UTCTime

instance Binary WrapUTCTime where
  get = iso8601ParseM =<< get
  put = put . iso8601Show

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: SourceMeta -> Maybe Int
latestRevisionNumber sm =
  if null (sourceRevisions sm)
    then Nothing
    else Just $ maximum $ map revisionNumber (sourceRevisions sm)
