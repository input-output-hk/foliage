{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Meta
  ( packageVersionTimestamp,
    packageVersionSource,
    packageVersionRevisions,
    packageVersionDeprecations,
    packageVersionForce,
    PackageVersionSpec (PackageVersionSpec),
    readPackageVersionSpec,
    writePackageVersionSpec,
    RevisionSpec (RevisionSpec),
    revisionTimestamp,
    revisionNumber,
    DeprecationSpec (DeprecationSpec),
    deprecationTimestamp,
    deprecationIsDeprecated,
    PackageVersionSource,
    pattern URISource,
    pattern GitHubSource,
    GitHubRepo (..),
    GitHubRev (..),
    UTCTime,
    latestRevisionNumber,
    packageVersionSourceToUri,
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
import Distribution.Types.Orphans ()
import Foliage.Time (UTCTime)
import GHC.Generics (Generic)
import Network.URI (URI (..), nullURI, parseURI)
import Network.URI.Orphans ()
import System.FilePath ((</>))
import Toml (TomlCodec, (.=))
import Toml qualified

newtype GitHubRepo = GitHubRepo {unGitHubRepo :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

newtype GitHubRev = GitHubRev {unGitHubRev :: Text}
  deriving (Show, Eq, Binary, Hashable, NFData) via Text

data PackageVersionSource
  = URISource
      { sourceURI :: URI,
        subdir :: Maybe String
      }
  | GitHubSource
      { githubRepo :: GitHubRepo,
        githubRev :: GitHubRev,
        subdir :: Maybe String
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

packageVersionSourceToUri :: PackageVersionSource -> URI
packageVersionSourceToUri (URISource uri Nothing) = uri
packageVersionSourceToUri (URISource uri (Just subdir)) = uri {uriQuery = "?dir=" ++ subdir}
packageVersionSourceToUri (GitHubSource repo rev Nothing) =
  nullURI
    { uriScheme = "github:",
      uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev)
    }
packageVersionSourceToUri (GitHubSource repo rev (Just subdir)) =
  nullURI
    { uriScheme = "github:",
      uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev),
      uriQuery = "?dir=" ++ subdir
    }

packageSourceCodec :: TomlCodec PackageVersionSource
packageSourceCodec =
  Toml.dimatch matchTarballSource (uncurry URISource) tarballSourceCodec
    <|> Toml.dimatch matchGitHubSource (\((repo, rev), mSubdir) -> GitHubSource repo rev mSubdir) githubSourceCodec

uriCodec :: Toml.Key -> TomlCodec URI
uriCodec = Toml.textBy to from
  where
    to = T.pack . show
    from t = case parseURI (T.unpack t) of
      Nothing -> Left $ "Invalid url: " <> t
      Just uri' -> Right uri'

tarballSourceCodec :: TomlCodec (URI, Maybe String)
tarballSourceCodec =
  Toml.pair
    (uriCodec "url")
    (Toml.dioptional $ Toml.string "subdir")

matchTarballSource :: PackageVersionSource -> Maybe (URI, Maybe String)
matchTarballSource (URISource url mSubdir) = Just (url, mSubdir)
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
    -- | deprecations
    packageVersionDeprecations :: [DeprecationSpec],
    -- | force version
    packageVersionForce :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

sourceMetaCodec :: TomlCodec PackageVersionSpec
sourceMetaCodec =
  PackageVersionSpec
    <$> Toml.dioptional (timeCodec "timestamp")
      .= packageVersionTimestamp
    <*> packageSourceCodec
      .= packageVersionSource
    <*> Toml.list revisionMetaCodec "revisions"
      .= packageVersionRevisions
    <*> Toml.list deprecationMetaCodec "deprecations"
      .= packageVersionDeprecations
    <*> withDefault False (Toml.bool "force-version")
      .= packageVersionForce

readPackageVersionSpec :: FilePath -> IO PackageVersionSpec
readPackageVersionSpec = Toml.decodeFile sourceMetaCodec

writePackageVersionSpec :: FilePath -> PackageVersionSpec -> IO ()
writePackageVersionSpec fp a = void $ Toml.encodeToFile sourceMetaCodec fp a

data RevisionSpec = RevisionSpec
  { revisionTimestamp :: UTCTime,
    revisionNumber :: Int
  }
  deriving (Show, Eq, Generic, Ord)
  deriving anyclass (Binary, Hashable, NFData)

revisionMetaCodec :: TomlCodec RevisionSpec
revisionMetaCodec =
  RevisionSpec
    <$> timeCodec "timestamp"
      .= revisionTimestamp
    <*> Toml.int "number"
      .= revisionNumber

data DeprecationSpec = DeprecationSpec
  { deprecationTimestamp :: UTCTime,
    -- | 'True' means the package version has been deprecated
    --   'False' means the package version has been undeprecated
    --   FIXME: we should consider something better than 'Bool'
    deprecationIsDeprecated :: Bool
  }
  deriving (Show, Eq, Generic, Ord)
  deriving anyclass (Binary, Hashable, NFData)

deprecationMetaCodec :: TomlCodec DeprecationSpec
deprecationMetaCodec =
  DeprecationSpec
    <$> timeCodec "timestamp"
      .= deprecationTimestamp
    <*> withDefault True (Toml.bool "deprecated")
      .= deprecationIsDeprecated

timeCodec :: Toml.Key -> TomlCodec UTCTime
timeCodec key = Toml.dimap (utcToZonedTime utc) zonedTimeToUTC $ Toml.zonedTime key

latestRevisionNumber :: PackageVersionSpec -> Maybe Int
latestRevisionNumber sm =
  case sortOn (Down . revisionNumber) (packageVersionRevisions sm) of
    [] -> Nothing
    rev : _ -> Just (revisionNumber rev)

withDefault :: (Eq a) => a -> TomlCodec a -> TomlCodec a
withDefault d c = (fromMaybe d <$> Toml.dioptional c) .= f
  where
    f a = if a == d then Nothing else Just a
