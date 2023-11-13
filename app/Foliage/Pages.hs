{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Pages (
  allPackagesPageTemplate,
  allPackageVersionsPageTemplate,
  packageVersionPageTemplate,
  makeAllPackagesPage,
  makePackageVersionPage,
  makeAllPackageVersionsPage,
  makeIndexPage,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Foldable1 (minimumBy)
import Data.Function (on, (&))
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text.Lazy.IO.Utf8 qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Development.Shake (Action, askOracle)
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Simple (PackageId, PackageIdentifier (..))
import Distribution.Utils.Generic (sndOf3)
import Foliage.Meta (PackageVersionSource, PackageVersionSpec (..), RevisionSpec (..), packageVersionIsDeprecated)
import Foliage.Meta.Aeson ()
import Foliage.Oracles (CurrentTime (..))
import Foliage.Utils.Aeson (MyAesonEncoding (..))
import GHC.Generics (Generic)
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Text.Mustache.Render (renderMustache)

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

makeAllPackagesPage :: FilePath -> [(FilePath, PackageId, PackageVersionSpec)] -> Action ()
makeAllPackagesPage path allPkgSpecs = do
  currentTime <- askOracle CurrentTime

  let packages :: [AllPackagesPageEntry]
      packages =
        allPkgSpecs
          -- group package versions by package name
          & NE.groupBy ((==) `on` sndOf3)
          -- for each package name pick the most recent version
          & map
            ( \group ->
                -- pick the most recent version
                minimumBy (comparing $ Down . pkgVersion . sndOf3) group
                  -- turn it into the template data
                  & ( \(_metaFile, pkgId, PackageVersionSpec{packageVersionTimestamp, packageVersionRevisions, packageVersionSource}) ->
                        AllPackagesPageEntry
                          { allPackagesPageEntryPkgId = pkgId
                          , allPackagesPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp
                          , allPackagesPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp)
                          , allPackagesPageEntrySource = packageVersionSource
                          , allPackagesPageEntryLatestRevisionTimestamp = revisionTimestamp <$> listToMaybe packageVersionRevisions
                          }
                    )
            )
          -- sort packages by pkgId
          & sortOn allPackagesPageEntryPkgId

  liftIO $
    TL.writeFile path $
      renderMustache allPackagesPageTemplate $
        object ["packages" .= packages]

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

makeAllPackageVersionsPage :: FilePath -> [(FilePath, PackageId, PackageVersionSpec)] -> Action ()
makeAllPackageVersionsPage path allPkgSpecs = do
  currentTime <- askOracle CurrentTime

  let entries =
        allPkgSpecs
          -- collect all cabal file revisions including the original cabal file
          & foldMap
            ( \(_metaFile, pkgId, pkgSpec@PackageVersionSpec{packageVersionSource, packageVersionTimestamp, packageVersionRevisions}) ->
                -- original cabal file
                AllPackageVersionsPageEntryPackage
                  { allPackageVersionsPageEntryPkgId = pkgId
                  , allPackageVersionsPageEntryTimestamp = fromMaybe currentTime packageVersionTimestamp
                  , allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds (fromMaybe currentTime packageVersionTimestamp)
                  , allPackageVersionsPageEntrySource = packageVersionSource
                  , allPackageVersionsPageEntryDeprecated = packageVersionIsDeprecated pkgSpec
                  }
                  -- list of revisions
                  : [ AllPackageVersionsPageEntryRevision
                      { allPackageVersionsPageEntryPkgId = pkgId
                      , allPackageVersionsPageEntryTimestamp = revisionTimestamp
                      , allPackageVersionsPageEntryTimestampPosix = utcTimeToPOSIXSeconds revisionTimestamp
                      , allPackageVersionsPageEntryDeprecated = packageVersionIsDeprecated pkgSpec
                      }
                    | RevisionSpec{revisionTimestamp} <- packageVersionRevisions
                    ]
            )
          -- sort them by timestamp
          & sortOn (Down . allPackageVersionsPageEntryTimestamp)

  liftIO $
    TL.writeFile path $
      renderMustache allPackageVersionsPageTemplate $
        object ["entries" .= entries]

makePackageVersionPage :: FilePath -> GenericPackageDescription -> PackageVersionSpec -> Action ()
makePackageVersionPage
  path
  pkgDesc
  pkgSpec@PackageVersionSpec{packageVersionSource, packageVersionRevisions, packageVersionTimestamp} =
    let
     in liftIO $
          TL.writeFile path $
            renderMustache packageVersionPageTemplate $
              object
                [ "pkgVersionSource" .= packageVersionSource
                , "cabalFileRevisions" .= map revisionTimestamp packageVersionRevisions
                , "pkgDesc" .= jsonGenericPackageDescription pkgDesc
                , "pkgTimestamp" .= packageVersionTimestamp
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
