{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    pattern GitHubSource,
    GitHubRepo (..),
    GitHubRev (..),
    UTCTime,
    latestRevisionNumber,
    consolidateRanges,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Text (Text)
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
import Network.URI
import Network.URI.Orphans ()
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

instance Aeson.ToJSON Version

instance Aeson.ToJSON VersionRange

instance Aeson.ToJSON PackageMetaEntry

instance Aeson.ToJSON PackageMeta

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

newtype GitHubRepo = GitHubRepo {unGitHubRepo :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

deriving via Text instance Aeson.ToJSON GitHubRepo

newtype GitHubRev = GitHubRev {unGitHubRev :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

deriving via Text instance Aeson.ToJSON GitHubRev

data PackageVersionSource
  = TarballSource
      { tarballSourceURI :: URI,
        subdirs :: Maybe String
      }
  | GitHubSource
      { githubRepo :: GitHubRepo,
        githubRev :: GitHubRev,
        subdirs :: Maybe String
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

instance Aeson.ToJSON PackageVersionSource where
  toJSON =
    Aeson.genericToJSON
      Aeson.defaultOptions
        { Aeson.sumEncoding = Aeson.ObjectWithSingleField
        }

instance Aeson.ToJSON URI where
  toJSON = Aeson.toJSON . show

packageSourceCodec :: TomlCodec PackageVersionSource
packageSourceCodec =
  Toml.dimatch matchTarballSource (uncurry TarballSource) tarballSourceCodec
    <|> Toml.dimatch matchGitHubSource (\((repo, rev), mSubdir) -> GitHubSource repo rev mSubdir) githubSourceCodec

uri :: Toml.Key -> TomlCodec URI
uri = Toml.textBy to from
  where
    to = T.pack . show
    from t = case parseURI (T.unpack t) of
      Nothing -> Left $ "Invalid url: " <> t
      Just uri' -> Right uri'

tarballSourceCodec :: TomlCodec (URI, Maybe String)
tarballSourceCodec =
  Toml.pair
    (uri "url")
    (Toml.dioptional $ Toml.string "subdir")

matchTarballSource :: PackageVersionSource -> Maybe (URI, Maybe String)
matchTarballSource (TarballSource url mSubdir) = Just (url, mSubdir)
matchTarballSource _ = Nothing

gitHubRepo :: Toml.Key -> TomlCodec GitHubRepo
gitHubRepo = Toml.dimap unGitHubRepo GitHubRepo . Toml.text

gitHubRev :: Toml.Key -> TomlCodec GitHubRev
gitHubRev = Toml.dimap unGitHubRev GitHubRev . Toml.text

githubSourceCodec :: TomlCodec ((GitHubRepo, GitHubRev), Maybe String)
githubSourceCodec =
  Toml.pair
    (Toml.table (Toml.pair (gitHubRepo "repo") (gitHubRev "rev")) "github")
    (Toml.dioptional $ Toml.string "subdir")

matchGitHubSource :: PackageVersionSource -> Maybe ((GitHubRepo, GitHubRev), Maybe String)
matchGitHubSource (GitHubSource repo rev mSubdir) = Just ((repo, rev), mSubdir)
matchGitHubSource _ = Nothing

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

instance Aeson.ToJSON PackageVersionMeta

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

instance Aeson.ToJSON RevisionMeta

revisionMetaCodec :: TomlCodec RevisionMeta
revisionMetaCodec =
  RevisionMeta
    <$> timeCodec "timestamp" .= revisionTimestamp
    <*> Toml.int "number" .= revisionNumber

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: PackageVersionMeta -> Maybe Int
latestRevisionNumber sm =
  case sortOn (Down . revisionNumber) (packageVersionRevisions sm) of
    [] -> Nothing
    rev : _ -> Just (revisionNumber rev)

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
