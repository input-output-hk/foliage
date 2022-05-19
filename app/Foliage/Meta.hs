{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Foliage.Meta
  ( PackageMeta (PackageMeta),
    readPackageMeta,
    writePackageMeta,
    PackageVersionMeta (PackageVersionMeta),
    packageVersionTimestamp,
    packageVersionSource,
    packageVersionRevisions,
    packageVersionForce,
    readPackageVersionMeta,
    writePackageVersionMeta,
    RevisionMeta (RevisionMeta),
    revisionTimestamp,
    revisionNumber,
    PackageVersionSource,
    pattern TarballSource,
    UTCTime,
    latestRevisionNumber,
    consolidateRanges,
  )
where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Development.Shake.Classes
  ( Binary,
    Hashable,
    NFData,
  )
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty
import Distribution.Types.Orphans ()
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange
  ( VersionRange,
    anyVersion,
    intersectVersionRanges,
    notThisVersion,
  )
import Distribution.Version
  ( isAnyVersion,
    isNoVersion,
    simplifyVersionRange,
  )
import Foliage.Time (UTCTime)
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import Toml qualified

newtype PackageMeta = PackageMeta
  { packageMetaEntries :: [PackageMetaEntry]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

data PackageMetaEntry = PackageMetaEntry
  { packageMetaEntryTimestamp :: UTCTime,
    packageMetaEntryPreferred :: [VersionRange],
    packageMetaEntryDeprecated :: [Version]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

readPackageMeta :: FilePath -> IO PackageMeta
readPackageMeta = Toml.decodeFile packageMetaCodec

writePackageMeta :: FilePath -> PackageMeta -> IO ()
writePackageMeta fp a = void $ Toml.encodeToFile packageMetaCodec fp a

packageMetaCodec :: TomlCodec PackageMeta
packageMetaCodec =
  PackageMeta
    <$> Toml.list packageMetaEntryCodec "entries" .= packageMetaEntries

packageMetaEntryCodec :: TomlCodec PackageMetaEntry
packageMetaEntryCodec =
  PackageMetaEntry
    <$> timeCodec "timestamp" .= packageMetaEntryTimestamp
    <*> Toml.arrayOf _VersionRange "preferred-versions" .= packageMetaEntryPreferred
    <*> Toml.arrayOf _Version "deprecated-versions" .= packageMetaEntryDeprecated

_Version :: Toml.TomlBiMap Version Toml.AnyValue
_Version = Toml._TextBy showVersion parseVersion
  where
    showVersion = T.pack . prettyShow
    parseVersion t = case simpleParsec (T.unpack t) of
      Nothing -> Left $ T.pack $ "unable to parse version" ++ T.unpack t
      Just v -> Right v

_VersionRange :: Toml.TomlBiMap VersionRange Toml.AnyValue
_VersionRange = Toml._TextBy showVersion parseVersion
  where
    showVersion = T.pack . prettyShow
    parseVersion t = case simpleParsec (T.unpack t) of
      Nothing -> Left $ T.pack $ "unable to parse version" ++ T.unpack t
      Just v -> Right v

data PackageVersionSource
  = TarballSource String (Maybe String)
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

packageSourceCodec :: TomlCodec PackageVersionSource
packageSourceCodec =
  Toml.dimatch matchTarballSource (uncurry TarballSource) tarballSourceCodec

tarballSourceCodec :: TomlCodec (String, Maybe String)
tarballSourceCodec =
  Toml.pair
    (Toml.string "url")
    (Toml.dioptional $ Toml.string "subdir")

matchTarballSource :: PackageVersionSource -> Maybe (String, Maybe String)
matchTarballSource (TarballSource url mSubdir) = Just (url, mSubdir)

data PackageVersionMeta = PackageVersionMeta
  { -- | timestamp
    packageVersionTimestamp :: Maybe UTCTime,
    -- | source parameters
    packageVersionSource :: PackageVersionSource,
    -- | revisions
    packageVersionRevisions :: [RevisionMeta],
    -- | force version
    packageVersionForce :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

sourceMetaCodec :: TomlCodec PackageVersionMeta
sourceMetaCodec =
  PackageVersionMeta
    <$> Toml.dioptional (timeCodec "timestamp") .= packageVersionTimestamp
    <*> packageSourceCodec .= packageVersionSource
    <*> Toml.list revisionMetaCodec "revisions" .= packageVersionRevisions
    <*> withDefault False (Toml.bool "force-version") .= packageVersionForce

readPackageVersionMeta :: FilePath -> IO PackageVersionMeta
readPackageVersionMeta = Toml.decodeFile sourceMetaCodec

writePackageVersionMeta :: FilePath -> PackageVersionMeta -> IO ()
writePackageVersionMeta fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionMeta = RevisionMeta
  { revisionTimestamp :: UTCTime,
    revisionNumber :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

revisionMetaCodec :: TomlCodec RevisionMeta
revisionMetaCodec =
  RevisionMeta
    <$> timeCodec "timestamp" .= revisionTimestamp
    <*> Toml.int "number" .= revisionNumber

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: PackageVersionMeta -> Maybe Int
latestRevisionNumber sm =
  if null (packageVersionRevisions sm)
    then Nothing
    else Just $ maximum $ map revisionNumber (packageVersionRevisions sm)

withDefault :: Eq a => a -> TomlCodec a -> TomlCodec a
withDefault d c = (fromMaybe d <$> Toml.dioptional c) .= f
  where
    f a = if a == d then Nothing else Just a

-- | copied from hackage-server
consolidateRanges :: PackageMetaEntry -> Maybe VersionRange
consolidateRanges PackageMetaEntry {packageMetaEntryPreferred, packageMetaEntryDeprecated} =
  if isAnyVersion range || isNoVersion range then Nothing else Just range
  where
    range =
      simplifyVersionRange $
        foldr intersectVersionRanges anyVersion (map notThisVersion packageMetaEntryDeprecated ++ packageMetaEntryPreferred)
