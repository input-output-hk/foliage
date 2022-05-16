{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Foliage.Meta
  ( PackageMeta,
    pattern PackageMeta,
    packageTimestamp,
    packageSource,
    packageRevisions,
    packageForceVersion,
    readPackageMeta,
    writePackageMeta,
    RevisionMeta,
    pattern RevisionMeta,
    revisionTimestamp,
    revisionNumber,
    PackageSource,
    pattern TarballSource,
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

data PackageSource
  = TarballSource String (Maybe String)
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

packageSourceCodec :: TomlCodec PackageSource
packageSourceCodec =
  Toml.dimatch matchTarballSource (uncurry TarballSource) tarballSourceCodec

tarballSourceCodec :: TomlCodec (String, Maybe String)
tarballSourceCodec =
  Toml.pair
    (Toml.string "url")
    (Toml.dioptional $ Toml.string "subdir")

matchTarballSource :: PackageSource -> Maybe (String, Maybe String)
matchTarballSource (TarballSource url mSubdir) = Just (url, mSubdir)

data PackageMeta
  = PackageMeta'
      (Maybe WrapUTCTime)
      -- ^ timestamp
      PackageSource
      -- ^ source parameters
      [RevisionMeta]
      -- ^ revisions
      Bool
      -- ^ force version
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern PackageMeta :: Maybe UTCTime -> PackageSource -> [RevisionMeta] -> Bool -> PackageMeta
pattern PackageMeta {packageTimestamp, packageSource, packageRevisions, packageForceVersion} <-
  PackageMeta' (coerce -> packageTimestamp) packageSource packageRevisions packageForceVersion
  where
    PackageMeta timestamp source revisions forceversion = PackageMeta' (coerce timestamp) source revisions forceversion

sourceMetaCodec :: TomlCodec PackageMeta
sourceMetaCodec =
  PackageMeta
    <$> Toml.dioptional (timeCodec "timestamp") .= packageTimestamp
    <*> packageSourceCodec .= packageSource
    <*> Toml.list revisionMetaCodec "revisions" .= packageRevisions
    <*> withDefault False (Toml.bool "force-version") .= packageForceVersion

readPackageMeta :: FilePath -> IO PackageMeta
readPackageMeta = Toml.decodeFile sourceMetaCodec

writePackageMeta :: FilePath -> PackageMeta -> IO ()
writePackageMeta fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionMeta = RevisionMeta' WrapUTCTime Int
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

pattern RevisionMeta :: UTCTime -> Int -> RevisionMeta
pattern RevisionMeta {revisionTimestamp, revisionNumber} <-
  RevisionMeta' (coerce -> revisionTimestamp) revisionNumber
  where
    RevisionMeta timestamp number = RevisionMeta' (coerce timestamp) number

revisionMetaCodec :: TomlCodec RevisionMeta
revisionMetaCodec =
  RevisionMeta
    <$> timeCodec "timestamp" .= revisionTimestamp
    <*> Toml.int "number" .= revisionNumber

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: PackageMeta -> Maybe Int
latestRevisionNumber sm =
  if null (packageRevisions sm)
    then Nothing
    else Just $ maximum $ map revisionNumber (packageRevisions sm)

withDefault :: Eq a => a -> TomlCodec a -> TomlCodec a
withDefault d c = (fromMaybe d <$> Toml.dioptional c) .= f
  where
    f a = if a == d then Nothing else Just a

newtype WrapUTCTime = WrapUTCTime {unwrapUTCTime :: UTCTime}
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving (ISO8601) via UTCTime

instance Binary WrapUTCTime where
  get = iso8601ParseM =<< get
  put = put . iso8601Show
