{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Rules.Pages (
  websitePagesRules,
) where

import Data.Foldable1 (minimumBy)
import Data.Function (on, (&))
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import GHC.Generics (Generic)

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Text.Lazy.IO.Utf8 qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Aeson (jsonGenericPackageDescription)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageId, PackageIdentifier (..))
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Utils.Generic (sndOf3)
import Distribution.Verbosity qualified as Verbosity
import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)
import Text.Mustache.Render (renderMustache)

import Foliage.Meta (
  PackageVersionSource,
  PackageVersionSpec (..),
  RevisionSpec (..),
  packageVersionIsDeprecated,
 )
import Foliage.Meta.Aeson ()
import Foliage.Utils.Aeson (MyAesonEncoding (..))

websitePagesRules
  :: FilePath
  -> UTCTime
  -> Action [(FilePath, PackageId, PackageVersionSpec)]
  -> (PackageId -> FilePath)
  -> (FilePath -> Action PackageVersionSpec)
  -> (PackageId -> FilePath)
  -> Rules ()
websitePagesRules outputDir currentTime getPkgSpecs metaFileForPkgId readPackageVersionSpec cabalFileForPkgId = do
  action $ do
    need
      [ outputDir </> "index.html"
      , outputDir </> "all-packages/index.html"
      , outputDir </> "all-package-versions/index.html"
      ]
    getPkgSpecs >>= \pkgSpecs ->
      need
        [ outputDir </> "package" </> prettyShow pkgId </> "index.html"
        | (_, pkgId, _) <- pkgSpecs
        ]

  outputDir </> "index.html"
    %> makeIndexPage

  outputDir </> "all-packages/index.html" %> \path ->
    getPkgSpecs >>= makeAllPackagesPage currentTime path

  outputDir </> "all-package-versions/index.html" %> \path ->
    getPkgSpecs >>= makeAllPackageVersionsPage currentTime path

  outputDir </> "package/*/index.html" %> \path ->
    case filePattern (outputDir </> "package/*/index.html") path of
      Just [pkgIdStr] | Just pkgId <- simpleParsec pkgIdStr -> do
        let metaFile = metaFileForPkgId pkgId
        pkgSpec <- readPackageVersionSpec metaFile
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

makeAllPackagesPage :: UTCTime -> FilePath -> [(FilePath, PackageId, PackageVersionSpec)] -> Action ()
makeAllPackagesPage currentTime path allPkgSpecs = do
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

makeAllPackageVersionsPage :: UTCTime -> FilePath -> [(FilePath, PackageId, PackageVersionSpec)] -> Action ()
makeAllPackageVersionsPage currentTime path allPkgSpecs = do
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
