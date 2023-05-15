{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Pages
  ( allPackagesPageTemplate,
    allPackageVersionsPageTemplate,
    packageVersionPageTemplate,
    makeAllPackagesPage,
    makePackageVersionPage,
    makeAllPackageVersionsPage,
    makeIndexPage,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Function (on, (&))
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text.Lazy.IO.Utf8 qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Development.Shake (Action, traced)
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.Package (PackageIdentifier (pkgName, pkgVersion))
import Distribution.Pretty (prettyShow)
import Foliage.Meta (PackageVersionSource)
import Foliage.Meta.Aeson ()
import Foliage.PreparePackageVersion (PreparedPackageVersion (..))
import Foliage.Utils.Aeson (MyAesonEncoding (..))
import GHC.Generics (Generic)
import System.Directory qualified as IO
import System.FilePath ((</>))
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Text.Mustache.Render (renderMustache)

makeIndexPage :: FilePath -> Action ()
makeIndexPage outputDir =
  traced "webpages / index" $ do
    IO.createDirectoryIfMissing True outputDir
    TL.writeFile (outputDir </> "index.html") $
      renderMustache indexPageTemplate $
        object []

data AllPackagesPageEntry = AllPackagesPageEntry
  { allPackagesPageEntryPkgId :: PackageIdentifier,
    allPackagesPageEntryTimestamp :: UTCTime,
    allPackagesPageEntryTimestampPosix :: POSIXTime,
    allPackagesPageEntrySource :: PackageVersionSource,
    allPackagesPageEntryLatestRevisionTimestamp :: Maybe UTCTime
  }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackagesPageEntry

makeAllPackagesPage :: UTCTime -> FilePath -> [PreparedPackageVersion] -> Action ()
makeAllPackagesPage currentTime outputDir packageVersions =
  traced "webpages / all-packages" $ do
    IO.createDirectoryIfMissing True (outputDir </> "all-packages")
    TL.writeFile (outputDir </> "all-packages" </> "index.html") $
      renderMustache allPackagesPageTemplate $
        object ["packages" .= packages]
  where
    packages =
      packageVersions
        -- group package versions by package name
        & NE.groupBy ((==) `on` (pkgName . pkgId))
        -- for each package name pick the most recent version
        & map
          ( \group ->
              group
                -- sort them from the most recent version to the least recent
                & NE.sortBy (comparing $ Down . pkgVersion . pkgId)
                -- pick the most recent version
                & NE.head
                -- turn it into the template data
                & ( \(PreparedPackageVersion {pkgId, pkgTimestamp, cabalFileRevisions, pkgVersionSource}) ->
                      AllPackagesPageEntry
                        { allPackagesPageEntryPkgId = pkgId,
                          allPackagesPageEntryTimestamp = fromMaybe currentTime pkgTimestamp,
                          allPackagesPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime pkgTimestamp),
                          allPackagesPageEntrySource = pkgVersionSource,
                          allPackagesPageEntryLatestRevisionTimestamp = fst <$> listToMaybe cabalFileRevisions
                        }
                  )
          )
        -- sort packages by pkgId
        & sortOn allPackagesPageEntryPkgId

data AllPackageVersionsPageEntry
  = AllPackageVersionsPageEntryPackage
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier,
        allPackageVersionsPageEntryTimestamp :: UTCTime,
        allPackageVersionsPageEntryTimestampPosix :: POSIXTime,
        allPackageVersionsPageEntrySource :: PackageVersionSource,
        allPackageVersionsPageEntryDeprecated :: Bool
      }
  | AllPackageVersionsPageEntryRevision
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier,
        allPackageVersionsPageEntryTimestamp :: UTCTime,
        allPackageVersionsPageEntryTimestampPosix :: POSIXTime
      }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackageVersionsPageEntry

makeAllPackageVersionsPage :: UTCTime -> FilePath -> [PreparedPackageVersion] -> Action ()
makeAllPackageVersionsPage currentTime outputDir packageVersions =
  traced "webpages / all-package-versions" $ do
    IO.createDirectoryIfMissing True (outputDir </> "all-package-versions")
    TL.writeFile (outputDir </> "all-package-versions" </> "index.html") $
      renderMustache allPackageVersionsPageTemplate $
        object ["entries" .= entries]
  where
    entries =
      -- collect all cabal file revisions including the original cabal file
      foldMap
        ( \PreparedPackageVersion {pkgId, pkgTimestamp, pkgVersionSource, pkgVersionIsDeprecated, cabalFileRevisions} ->
            -- original cabal file
            AllPackageVersionsPageEntryPackage
              { allPackageVersionsPageEntryPkgId = pkgId,
                allPackageVersionsPageEntryTimestamp = fromMaybe currentTime pkgTimestamp,
                allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime pkgTimestamp),
                allPackageVersionsPageEntrySource = pkgVersionSource,
                allPackageVersionsPageEntryDeprecated = pkgVersionIsDeprecated
              }
              -- list of revisions
              : [ AllPackageVersionsPageEntryRevision
                    { allPackageVersionsPageEntryPkgId = pkgId,
                      allPackageVersionsPageEntryTimestamp = revisionTimestamp,
                      allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds revisionTimestamp
                    }
                  | (revisionTimestamp, _) <- cabalFileRevisions
                ]
        )
        packageVersions
        -- sort them by timestamp
        & sortOn (Down . allPackageVersionsPageEntryTimestamp)

makePackageVersionPage :: FilePath -> PreparedPackageVersion -> Action ()
makePackageVersionPage outputDir PreparedPackageVersion {pkgId, pkgTimestamp, pkgVersionSource, pkgDesc, cabalFileRevisions, pkgVersionIsDeprecated} = do
  traced ("webpages / package / " ++ prettyShow pkgId) $ do
    IO.createDirectoryIfMissing True (outputDir </> "package" </> prettyShow pkgId)
    TL.writeFile (outputDir </> "package" </> prettyShow pkgId </> "index.html") $
      renderMustache packageVersionPageTemplate $
        object
          [ "pkgVersionSource" .= pkgVersionSource,
            "cabalFileRevisions" .= map fst cabalFileRevisions,
            "pkgDesc" .= jsonGenericPackageDescription pkgDesc,
            "pkgTimestamp" .= pkgTimestamp,
            "pkgVersionDeprecated" .= pkgVersionIsDeprecated
          ]

indexPageTemplate :: Template
indexPageTemplate = $(compileMustacheDir "index" "templates")

allPackagesPageTemplate :: Template
allPackagesPageTemplate = $(compileMustacheDir "allPackages" "templates")

allPackageVersionsPageTemplate :: Template
allPackageVersionsPageTemplate = $(compileMustacheDir "allPackageVersions" "templates")

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
