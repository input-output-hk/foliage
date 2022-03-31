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
    sourceForceVersion,
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
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Time.Format.ISO8601
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Development.Shake.Classes
import Foliage.Time
import GHC.Generics
import Toml (TomlCodec, (.=))
import Toml qualified

data SourceMeta
  = SourceMeta'
      (Maybe WrapUTCTime)
      -- ^ timestamp
      String
      -- ^ url
      (Maybe String)
      -- ^ subdir
      [RevisionMeta]
      -- ^ revisions
      Bool
      -- ^ force version
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern SourceMeta :: Maybe UTCTime -> String -> Maybe String -> [RevisionMeta] -> Bool -> SourceMeta
pattern SourceMeta {sourceTimestamp, sourceUrl, sourceSubdir, sourceRevisions, sourceForceVersion} <-
  SourceMeta' (coerce -> sourceTimestamp) sourceUrl sourceSubdir sourceRevisions sourceForceVersion
  where
    SourceMeta timestamp url subdir revisions forceversion = SourceMeta' (coerce timestamp) url subdir revisions forceversion

sourceMetaCodec :: TomlCodec SourceMeta
sourceMetaCodec =
  SourceMeta
    <$> Toml.dioptional (timeCodec "timestamp") .= sourceTimestamp
    <*> Toml.string "url" .= sourceUrl
    <*> Toml.dioptional (Toml.string "subdir") .= sourceSubdir
    <*> Toml.list revisionMetaCodec "revisions" .= sourceRevisions
    <*> withDefault False (Toml.bool "force-version") .= sourceForceVersion

withDefault :: Eq a => a -> TomlCodec a -> TomlCodec a
withDefault d c = (fromMaybe d <$> Toml.dioptional c) .= f
  where
    f a = if a == d then Nothing else Just a

readSourceMeta :: FilePath -> IO SourceMeta
readSourceMeta = Toml.decodeFile sourceMetaCodec

writeSourceMeta :: FilePath -> SourceMeta -> IO ()
writeSourceMeta fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionMeta = RevisionMeta' (Maybe WrapUTCTime) Int
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern RevisionMeta :: Maybe UTCTime -> Int -> RevisionMeta
pattern RevisionMeta {revisionTimestamp, revisionNumber} <-
  RevisionMeta' (coerce -> revisionTimestamp) revisionNumber
  where
    RevisionMeta timestamp number = RevisionMeta' (coerce timestamp) number

revisionMetaCodec :: TomlCodec RevisionMeta
revisionMetaCodec =
  RevisionMeta
    <$> Toml.dioptional (timeCodec "timestamp") .= revisionTimestamp
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
