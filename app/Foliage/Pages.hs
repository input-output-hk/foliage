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
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down))
import Data.Text.Lazy.IO.Utf8 qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Development.Shake (Action, traced)
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.Package (PackageId, PackageIdentifier (pkgName, pkgVersion))
import Distribution.Pretty (prettyShow)
import Foliage.Meta (PackageVersionSource, PackageVersionSpec (PackageVersionSpec, packageVersionRevisions, packageVersionSource, packageVersionTimestamp), RevisionSpec (RevisionSpec, revisionTimestamp), revisedCabalFile)
import Foliage.Meta.Aeson ()
import Foliage.Shake (originalCabalFile, readGenericPackageDescription')
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
    allPackagesPageEntryRevision :: Maybe RevisionSpec
  }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackagesPageEntry

makeAllPackagesPage :: UTCTime -> FilePath -> [(PackageId, PackageVersionSpec)] -> Action ()
makeAllPackagesPage currentTime outputDir packageVersions = do
  let packages =
        sortOn allPackagesPageEntryPkgId
          $ map
            ( ( \(pkgId, PackageVersionSpec {packageVersionTimestamp, packageVersionRevisions, packageVersionSource}) ->
                  AllPackagesPageEntry
                    { allPackagesPageEntryPkgId = pkgId,
                      allPackagesPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp,
                      allPackagesPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp),
                      allPackagesPageEntrySource = packageVersionSource,
                      allPackagesPageEntryRevision = listToMaybe packageVersionRevisions
                    }
              )
                . head
                . sortOn (Down . pkgVersion . fst)
            )
          $ groupBy ((==) `on` (pkgName . fst)) packageVersions
  traced "webpages / all-packages" $ do
    IO.createDirectoryIfMissing True (outputDir </> "all-packages")
    TL.writeFile (outputDir </> "all-packages" </> "index.html") $
      renderMustache allPackagesPageTemplate $
        object ["packages" .= packages]

data AllPackageVersionsPageEntry
  = AllPackageVersionsPageEntryPackage
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier,
        allPackageVersionsPageEntryTimestamp :: UTCTime,
        allPackageVersionsPageEntryTimestampPosix :: POSIXTime,
        allPackageVersionsPageEntrySource :: PackageVersionSource
      }
  | AllPackageVersionsPageEntryRevision
      { allPackageVersionsPageEntryPkgId :: PackageIdentifier,
        allPackageVersionsPageEntryTimestamp :: UTCTime,
        allPackageVersionsPageEntryTimestampPosix :: POSIXTime
      }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding AllPackageVersionsPageEntry

makeAllPackageVersionsPage :: UTCTime -> FilePath -> [(PackageId, PackageVersionSpec)] -> Action ()
makeAllPackageVersionsPage currentTime outputDir packageVersions = do
  let entries =
        sortOn (Down . allPackageVersionsPageEntryTimestamp) $
          foldMap
            ( \(pkgId, PackageVersionSpec {packageVersionTimestamp, packageVersionRevisions, packageVersionSource}) ->
                AllPackageVersionsPageEntryPackage
                  { allPackageVersionsPageEntryPkgId = pkgId,
                    allPackageVersionsPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp,
                    allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp),
                    allPackageVersionsPageEntrySource = packageVersionSource
                  }
                  : map
                    ( \RevisionSpec {revisionTimestamp} ->
                        AllPackageVersionsPageEntryRevision
                          { allPackageVersionsPageEntryPkgId = pkgId,
                            allPackageVersionsPageEntryTimestamp = revisionTimestamp,
                            allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds revisionTimestamp
                          }
                    )
                    packageVersionRevisions
            )
            packageVersions

  traced "webpages / all-package-versions" $ do
    IO.createDirectoryIfMissing True (outputDir </> "all-package-versions")
    TL.writeFile (outputDir </> "all-package-versions" </> "index.html") $
      renderMustache allPackageVersionsPageTemplate $
        object ["entries" .= entries]

makePackageVersionPage :: FilePath -> FilePath -> PackageId -> PackageVersionSpec -> Action ()
makePackageVersionPage inputDir outputDir pkgId pkgSpec = do
  cabalFilePath <- maybe (originalCabalFile pkgId pkgSpec) pure (revisedCabalFile inputDir pkgId pkgSpec)
  pkgDesc <- readGenericPackageDescription' cabalFilePath
  traced ("webpages / package / " ++ prettyShow pkgId) $ do
    IO.createDirectoryIfMissing True (outputDir </> "package" </> prettyShow pkgId)
    TL.writeFile (outputDir </> "package" </> prettyShow pkgId </> "index.html") $
      renderMustache packageVersionPageTemplate $
        object
          [ "pkgSpec" .= pkgSpec,
            "pkgDesc" .= jsonGenericPackageDescription pkgDesc
          ]

indexPageTemplate :: Template
indexPageTemplate = $(compileMustacheDir "index" "templates")

allPackagesPageTemplate :: Template
allPackagesPageTemplate = $(compileMustacheDir "allPackages" "templates")

allPackageVersionsPageTemplate :: Template
allPackageVersionsPageTemplate = $(compileMustacheDir "allPackageVersions" "templates")

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
