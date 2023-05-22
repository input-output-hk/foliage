{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Meta (
  packageVersionTimestamp,
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
import Control.Monad.State (modify)
import Data.HashMap.Strict qualified as HashMap
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
      { sourceURI :: URI
      , subdir :: Maybe String
      }
  | GitHubSource
      { githubRepo :: GitHubRepo
      , githubRev :: GitHubRev
      , subdir :: Maybe String
      }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

packageVersionSourceToUri :: PackageVersionSource -> URI
packageVersionSourceToUri (URISource uri Nothing) = uri
packageVersionSourceToUri (URISource uri (Just subdir)) = uri{uriQuery = "?dir=" ++ subdir}
packageVersionSourceToUri (GitHubSource repo rev Nothing) =
  nullURI
    { uriScheme = "github:"
    , uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev)
    }
packageVersionSourceToUri (GitHubSource repo rev (Just subdir)) =
  nullURI
    { uriScheme = "github:"
    , uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev)
    , uriQuery = "?dir=" ++ subdir
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
  { packageVersionTimestamp :: Maybe UTCTime
  -- ^ timestamp
  , packageVersionSource :: PackageVersionSource
  -- ^ source parameters
  , packageVersionRevisions :: [RevisionSpec]
  -- ^ revisions
  , packageVersionDeprecations :: [DeprecationSpec]
  -- ^ deprecations
  , packageVersionForce :: Bool
  -- ^ force version
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

sourceMetaCodec :: TomlCodec PackageVersionSpec
sourceMetaCodec =
  PackageVersionSpec
    <$> optionalTimeCodec "timestamp"
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
  { revisionTimestamp :: UTCTime
  , revisionNumber :: Int
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
  { deprecationTimestamp :: UTCTime
  , deprecationIsDeprecated :: Bool
  -- ^ 'True' means the package version has been deprecated
  --   'False' means the package version has been undeprecated
  --   FIXME: we should consider something better than 'Bool'
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

{- | Codec for a maybe-missing time value.

Note this is different from dioptional timeCodec. With dioptional timeCodec,
if the user writes
    timestamp = '2022-08-22T10:38:45Z'
rather then
    timestamp = 2022-08-22T10:38:45Z
the timestamp will parse as Nothing because it won't match the zoneTime
type and it is not an error because it is optional.

We use a handrolled version of match (matchMaybe) to make it work.

See discussions at
 1. https://github.com/input-output-hk/foliage/issues/11
 2. https://github.com/input-output-hk/foliage/pull/57
 3. https://github.com/kowainik/tomland/issues/223
-}
optionalTimeCodec :: Toml.Key -> TomlCodec (Maybe UTCTime)
optionalTimeCodec key =
  Toml.dimap (fmap $ utcToZonedTime utc) (fmap zonedTimeToUTC) $ matchMaybe Toml._ZonedTime key

matchMaybe :: forall a. Toml.TomlBiMap a Toml.AnyValue -> Toml.Key -> TomlCodec (Maybe a)
matchMaybe bimap key = Toml.Codec input output
 where
  input :: Toml.TomlEnv (Maybe a)
  input toml = case HashMap.lookup key (Toml.tomlPairs toml) of
    Nothing -> pure Nothing
    Just anyVal -> pure <$> Toml.whenLeftBiMapError key (Toml.backward bimap anyVal) pure

  output :: Maybe a -> Toml.TomlState (Maybe a)
  output Nothing = pure Nothing
  output (Just a) = do
    anyVal <- Toml.eitherToTomlState $ Toml.forward bimap a
    Just a <$ modify (Toml.insertKeyAnyVal key anyVal)
