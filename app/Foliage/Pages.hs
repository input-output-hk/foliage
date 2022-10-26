{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Pages
  ( contentsPageTemplate,
    timelinePageTemplate,
    packageVersionPageTemplate,
    makeContentsPage,
    makePackageVersionPage,
    makeTimelinePage,
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
import Development.Shake (Action, liftIO)
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.Package (PackageIdentifier (pkgName, pkgVersion))
import Distribution.Pretty (prettyShow)
import Foliage.Meta (PackageVersionMeta (..), PackageVersionSource, PackageVersionSpec (PackageVersionSpec, packageVersionRevisions, packageVersionSource, packageVersionTimestamp), RevisionSpec (RevisionSpec, revisionTimestamp), revisedCabalFile)
import Foliage.Meta.Aeson ()
import Foliage.Shake (originalCabalFile, readGenericPackageDescription')
import Foliage.Utils.Aeson (MyAesonEncoding (..))
import GHC.Generics (Generic)
import System.Directory qualified as IO
import System.FilePath ((</>))
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Text.Mustache.Render (renderMustache)

data ContentsPageEntry = ContentsPageEntry
  { contentsPageEntryPkgId :: PackageIdentifier,
    contentsPageEntryTimestamp :: UTCTime,
    contentsPageEntryTimestampPosix :: POSIXTime,
    contentsPageEntrySource :: PackageVersionSource,
    contentsPageEntryRevision :: Maybe RevisionSpec
  }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding ContentsPageEntry

makeContentsPage :: UTCTime -> FilePath -> [PackageVersionMeta] -> Action ()
makeContentsPage currentTime outputDir packageVersions = do
  let packages =
        sortOn contentsPageEntryPkgId $
          map
            ( ( \PackageVersionMeta {pkgId, pkgSpec = PackageVersionSpec {packageVersionTimestamp, packageVersionRevisions, packageVersionSource}} ->
                  ContentsPageEntry
                    { contentsPageEntryPkgId = pkgId,
                      contentsPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp,
                      contentsPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp),
                      contentsPageEntrySource = packageVersionSource,
                      contentsPageEntryRevision = listToMaybe packageVersionRevisions
                    }
              )
                . head
                . sortOn (Down . pkgVersion . pkgId)
            )
            $ groupBy ((==) `on` (pkgName . pkgId)) packageVersions
  liftIO $ do
    IO.createDirectoryIfMissing True outputDir
    IO.createDirectoryIfMissing True (outputDir </> "contents")
    TL.writeFile (outputDir </> "contents" </> "index.html") $
      renderMustache contentsPageTemplate $
        object ["packages" .= packages]

data TimelinePageEntry
  = TimelinePageEntryPackage
      { timelinePageEntryPkgId :: PackageIdentifier,
        timelinePageEntryTimestamp :: UTCTime,
        timelinePageEntryTimestampPosix :: POSIXTime,
        timelinePageEntrySource :: PackageVersionSource
      }
  | TimelinePageEntryRevision
      { timelinePageEntryPkgId :: PackageIdentifier,
        timelinePageEntryTimestamp :: UTCTime,
        timelinePageEntryTimestampPosix :: POSIXTime
      }
  deriving stock (Generic)
  deriving (ToJSON) via MyAesonEncoding TimelinePageEntry

makeTimelinePage :: UTCTime -> FilePath -> [PackageVersionMeta] -> Action ()
makeTimelinePage currentTime outputDir packageVersions = do
  let entries =
        sortOn (Down . timelinePageEntryTimestamp) $
          foldMap
            ( \PackageVersionMeta {pkgId, pkgSpec = PackageVersionSpec {packageVersionTimestamp, packageVersionRevisions, packageVersionSource}} ->
                TimelinePageEntryPackage
                  { timelinePageEntryPkgId = pkgId,
                    timelinePageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp,
                    timelinePageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp),
                    timelinePageEntrySource = packageVersionSource
                  } :
                map
                  ( \RevisionSpec {revisionTimestamp} ->
                      TimelinePageEntryRevision
                        { timelinePageEntryPkgId = pkgId,
                          timelinePageEntryTimestamp = revisionTimestamp,
                          timelinePageEntryTimestampPosix = utcTimeToPOSIXSeconds revisionTimestamp
                        }
                  )
                  packageVersionRevisions
            )
            packageVersions

  liftIO $ do
    IO.createDirectoryIfMissing True outputDir
    IO.createDirectoryIfMissing True (outputDir </> "timeline")
    TL.writeFile (outputDir </> "timeline" </> "index.html") $
      renderMustache timelinePageTemplate $
        object ["entries" .= entries]

makePackageVersionPage :: FilePath -> FilePath -> PackageVersionMeta -> Action ()
makePackageVersionPage inputDir outputDir pkgMeta@PackageVersionMeta {pkgId, pkgSpec} = do
  cabalFilePath <- maybe (originalCabalFile pkgMeta) pure (revisedCabalFile inputDir pkgMeta)
  pkgDesc <- readGenericPackageDescription' cabalFilePath
  liftIO $ do
    IO.createDirectoryIfMissing True (outputDir </> "package" </> prettyShow pkgId)
    TL.writeFile (outputDir </> "package" </> prettyShow pkgId </> "index.html") $
      renderMustache packageVersionPageTemplate $
        object
          [ "pkgSpec" .= pkgSpec,
            "pkgDesc" .= jsonGenericPackageDescription pkgDesc
          ]

contentsPageTemplate :: Template
contentsPageTemplate = $(compileMustacheDir "contents" "templates")

timelinePageTemplate :: Template
timelinePageTemplate = $(compileMustacheDir "timeline" "templates")

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
