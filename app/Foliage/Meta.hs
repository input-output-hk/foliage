{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Meta
  ( PackageMeta (PackageMeta),
    PackageMetaEntry (PackageMetaEntry),
    packageMetaEntryDeprecated,
    packageMetaEntryPreferred,
    packageMetaEntryTimestamp,
    readPackageMeta,
    writePackageMeta,
    PackageVersionMeta (PackageVersionMeta, pkgId, pkgSpec),
    packageVersionTimestamp,
    packageVersionSource,
    packageVersionRevisions,
    packageVersionForce,
    PackageVersionSpec (PackageVersionSpec),
    readPackageVersionSpec,
    writePackageVersionSpec,
    RevisionSpec (RevisionSpec),
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
    cabalFileRevisionPath,
    revisedCabalFile,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.LocalTime (utc, utcToZonedTime, zonedTimeToUTC)
import Development.Shake.Classes (Binary, Hashable, NFData)
import Distribution.Aeson ()
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Orphans ()
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion, intersectVersionRanges, notThisVersion)
import Distribution.Version (isAnyVersion, isNoVersion, simplifyVersionRange)
import Foliage.Time (UTCTime)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)
import Network.URI.Orphans ()
import System.FilePath ((<.>), (</>))
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

newtype GitHubRepo = GitHubRepo {unGitHubRepo :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

newtype GitHubRev = GitHubRev {unGitHubRev :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

data PackageVersionSource
  = TarballSource
      { tarballSourceURI :: URI,
        subdir :: Maybe String
      }
  | GitHubSource
      { githubRepo :: GitHubRepo,
        githubRev :: GitHubRev,
        subdir :: Maybe String
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

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

data PackageVersionSpec = PackageVersionSpec
  { -- | timestamp
    packageVersionTimestamp :: Maybe UTCTime,
    -- | source parameters
    packageVersionSource :: PackageVersionSource,
    -- | revisions
    packageVersionRevisions :: [RevisionSpec],
    -- | force version
    packageVersionForce :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

sourceMetaCodec :: TomlCodec PackageVersionSpec
sourceMetaCodec =
  PackageVersionSpec
    <$> Toml.dioptional (timeCodec "timestamp") .= packageVersionTimestamp
    <*> packageSourceCodec .= packageVersionSource
    <*> Toml.list revisionMetaCodec "revisions" .= packageVersionRevisions
    <*> withDefault False (Toml.bool "force-version") .= packageVersionForce

readPackageVersionSpec :: FilePath -> IO PackageVersionSpec
readPackageVersionSpec = Toml.decodeFile sourceMetaCodec

writePackageVersionSpec :: FilePath -> PackageVersionSpec -> IO ()
writePackageVersionSpec fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionSpec = RevisionSpec
  { revisionTimestamp :: UTCTime,
    revisionNumber :: Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

revisionMetaCodec :: TomlCodec RevisionSpec
revisionMetaCodec =
  RevisionSpec
    <$> timeCodec "timestamp" .= revisionTimestamp
    <*> Toml.int "number" .= revisionNumber

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: PackageVersionSpec -> Maybe Int
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

data PackageVersionMeta = PackageVersionMeta
  { pkgId :: PackageIdentifier,
    pkgSpec :: PackageVersionSpec
  }
  deriving (Show, Eq)
  deriving (Generic)

cabalFileRevisionPath :: FilePath -> PackageIdentifier -> Int -> FilePath
cabalFileRevisionPath inputDir PackageIdentifier {pkgName, pkgVersion} revisionNumber =
  inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "revisions" </> show revisionNumber <.> "cabal"

revisedCabalFile :: FilePath -> PackageVersionMeta -> Maybe FilePath
revisedCabalFile inputDir PackageVersionMeta {pkgId, pkgSpec} = do
  cabalFileRevisionPath inputDir pkgId <$> latestRevisionNumber pkgSpec
