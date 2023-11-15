{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Rules.Pages (
  websitePagesRules,
) where

import Data.Foldable1 qualified as F1
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import GHC.Generics (Generic)

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Map qualified as M
import Data.Map.Strict (Map)
import Data.Text.Lazy.IO.Utf8 qualified as TL
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageId, PackageIdentifier (..))
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Text.Mustache.Render (renderMustache)

import Foliage.Meta
import Foliage.Meta.Aeson ()
import Foliage.Rules.Utils
import Foliage.Utils.Aeson

websitePagesRules
  :: FilePath
  -> UTCTime
  -> (PackageId -> FilePath)
  -> Rules ()
websitePagesRules outputDir currentTime cabalFileForPkgId = do
  action $ do
    need
      [ outputDir </> "index.html"
      , outputDir </> "all-packages/index.html"
      , outputDir </> "all-package-versions/index.html"
      ]
    askOracle PkgSpecs >>= \pkgSpecs ->
      need
        [ outputDir </> "package" </> prettyShow pkgId </> "index.html"
        | pkgId <- M.keys pkgSpecs
        ]

  outputDir </> "index.html"
    %> makeIndexPage

  outputDir </> "all-packages/index.html" %> \path ->
    askOracle PkgSpecs >>= makeAllPackagesPage currentTime path

  outputDir </> "all-package-versions/index.html" %> \path ->
    askOracle PkgSpecs >>= makeAllPackageVersionsPage currentTime path

  outputDir </> "package/*/index.html" %> \path ->
    case filePattern (outputDir </> "package/*/index.html") path of
      Just [pkgIdStr] | Just pkgId <- simpleParsec pkgIdStr -> do
        pkgSpec <- askOracle (PkgSpecFor pkgId)
        let cabalFile = cabalFileForPkgId pkgId
        need [cabalFile]
        pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.silent cabalFile
        makePackageVersionPage path pkgDesc pkgSpec
      _ -> error $ "The path " ++ path ++ " does not correspond to a valid package"

makeIndexPage :: FilePath -> Action ()
makeIndexPage path = do
  liftIO $
    TL.writeFile path $
      renderMustache indexPageTemplate $
        object []

data AllPackagesPageEntry = AllPackagesPageEntry
  { allPackagesPageEntryPkgId :: PackageIdentifier
  , allPackagesPageEntryTimestamp :: UTCTime
  , allPackagesPageEntryTimestampPosix :: POSIXTime
  , allPackagesPageEntrySource :: PackageVersionSource
  , allPackagesPageEntryLatestRevisionTimestamp :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackagesPageEntry

makeAllPackagesPage :: UTCTime -> FilePath -> Map PackageId (FilePath, PackageVersionSpec) -> Action ()
makeAllPackagesPage currentTime path pkgSpecs =
  liftIO $
    TL.writeFile path $
      renderMustache allPackagesPageTemplate $
        object ["packages" .= packages]
 where
  packages =
    sortOn
      allPackagesPageEntryPkgId
      [ AllPackagesPageEntry
        { allPackagesPageEntryPkgId = pkgId
        , allPackagesPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp
        , allPackagesPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp)
        , allPackagesPageEntrySource = packageVersionSource
        , allPackagesPageEntryLatestRevisionTimestamp = revisionTimestamp <$> listToMaybe packageVersionRevisions
        }
      | (pkgName, pvs) <- M.toList (groupByPackageName pkgSpecs)
      , let (pkgVersion, pkgSpec) = F1.minimumBy (comparing $ Down . fst) pvs
      , let PackageVersionSpec{packageVersionTimestamp, packageVersionRevisions, packageVersionSource} = pkgSpec
      , let pkgId = PackageIdentifier pkgName pkgVersion
      ]

-- FIXME: refactor this
data AllPackageVersionsPageEntry
  = AllPackageVersionsPageEntryPackage
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier
      , allPackageVersionsPageEntryTimestamp :: UTCTime
      , allPackageVersionsPageEntryTimestampPosix :: POSIXTime
      , allPackageVersionsPageEntrySource :: PackageVersionSource
      , allPackageVersionsPageEntryDeprecated :: Bool
      }
  | AllPackageVersionsPageEntryRevision
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier
      , allPackageVersionsPageEntryTimestamp :: UTCTime
      , allPackageVersionsPageEntryTimestampPosix :: POSIXTime
      , allPackageVersionsPageEntryDeprecated :: Bool
      }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackageVersionsPageEntry

makeAllPackageVersionsPage :: UTCTime -> FilePath -> Map PackageId (FilePath, PackageVersionSpec) -> Action ()
makeAllPackageVersionsPage currentTime path pkgSpecs =
  liftIO $
    TL.writeFile path $
      renderMustache allPackageVersionsPageTemplate $
        object ["entries" .= entries]
 where
  entries =
    sortOn (Down . allPackageVersionsPageEntryTimestamp)
      -- collect all cabal file revisions including the original cabal file
      . M.foldMapWithKey
        ( \pkgId (_, pkgSpec) ->
            -- original cabal file
            AllPackageVersionsPageEntryPackage
              { allPackageVersionsPageEntryPkgId = pkgId
              , allPackageVersionsPageEntryTimestamp = fromMaybe currentTime $ packageVersionTimestamp pkgSpec
              , allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds $ fromMaybe currentTime $ packageVersionTimestamp pkgSpec
              , allPackageVersionsPageEntrySource = packageVersionSource pkgSpec
              , allPackageVersionsPageEntryDeprecated = packageVersionIsDeprecated pkgSpec
              --
              }
              -- list of revisions
              : [ AllPackageVersionsPageEntryRevision
                  { allPackageVersionsPageEntryPkgId = pkgId
                  , allPackageVersionsPageEntryTimestamp = revisionTimestamp
                  , allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds revisionTimestamp
                  , allPackageVersionsPageEntryDeprecated = packageVersionIsDeprecated pkgSpec
                  }
                | RevisionSpec{revisionTimestamp} <- packageVersionRevisions pkgSpec
                ]
        )
      $ pkgSpecs

makePackageVersionPage :: FilePath -> GenericPackageDescription -> PackageVersionSpec -> Action ()
makePackageVersionPage path pkgDesc pkgSpec =
  liftIO $
    TL.writeFile path $
      renderMustache packageVersionPageTemplate $
        object
          [ "pkgVersionSource" .= packageVersionSource pkgSpec
          , "cabalFileRevisions" .= map revisionTimestamp (packageVersionRevisions pkgSpec)
          , "pkgDesc" .= jsonGenericPackageDescription pkgDesc
          , "pkgTimestamp" .= packageVersionTimestamp pkgSpec
          , "pkgVersionDeprecated" .= packageVersionIsDeprecated pkgSpec
          ]

indexPageTemplate :: Template
indexPageTemplate = $(compileMustacheDir "index" "templates")

allPackagesPageTemplate :: Template
allPackagesPageTemplate = $(compileMustacheDir "allPackages" "templates")

allPackageVersionsPageTemplate :: Template
allPackageVersionsPageTemplate = $(compileMustacheDir "allPackageVersions" "templates")

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
