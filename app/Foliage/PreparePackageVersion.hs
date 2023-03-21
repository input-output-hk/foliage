{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Foliage.PreparePackageVersion
  ( PreparedPackageVersion
      ( pkgId,
        pkgTimestamp,
        pkgVersionSource,
        pkgVersionForce,
        pkgDesc,
        sdistPath,
        cabalFilePath,
        originalCabalFilePath,
        cabalFileRevisions
      ),
    pattern PreparedPackageVersion,
    preparePackageVersion,
  )
where

import Control.Monad (unless)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Development.Shake (Action)
import Development.Shake.FilePath (joinPath, splitDirectories)
import Distribution.Client.Compat.Prelude (fromMaybe, prettyShow)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (packageDescription))
import Distribution.Types.PackageDescription (PackageDescription (package))
import Distribution.Types.PackageId
import Foliage.Meta (PackageVersionSource, PackageVersionSpec (..), RevisionSpec (..), UTCTime, latestRevisionNumber)
import Foliage.PrepareSdist (prepareSdist)
import Foliage.PrepareSource (prepareSource)
import Foliage.Shake (readGenericPackageDescription', readPackageVersionSpec')
import System.FilePath (takeBaseName, takeFileName, (<.>), (</>))

data PreparedPackageVersion = PreparedPackageVersion
  { pkgId :: PackageId,
    pkgTimestamp :: Maybe UTCTime,
    pkgVersionSource :: PackageVersionSource,
    pkgVersionForce :: Bool,
    pkgDesc :: GenericPackageDescription,
    sdistPath :: FilePath,
    cabalFilePath :: FilePath,
    originalCabalFilePath :: FilePath,
    cabalFileRevisions :: [(UTCTime, FilePath)]
  }

preparePackageVersion :: FilePath -> FilePath -> Action PreparedPackageVersion
preparePackageVersion inputDir metaFile = do
  let (name, version) = case splitDirectories metaFile of
        [n, v, _] -> (n, v)
        _else -> error $ "internal error: I should not be looking at " ++ metaFile

  let pkgName = fromMaybe (error $ "invalid package name: " ++ name) $ simpleParsec name
  let pkgVersion = fromMaybe (error $ "invalid package version: " ++ version) $ simpleParsec version
  let pkgId = PackageIdentifier pkgName pkgVersion

  pkgSpec <-
    readPackageVersionSpec' (inputDir </> metaFile) >>= \case
      PackageVersionSpec {packageVersionRevisions, packageVersionTimestamp = Nothing}
        | not (null packageVersionRevisions) -> do
            error $
              unlines
                [ inputDir </> metaFile <> " has cabal file revisions but the original package has no timestamp.",
                  "This combination doesn't make sense. Either add a timestamp on the original package or remove the revisions"
                ]
      PackageVersionSpec {packageVersionRevisions, packageVersionTimestamp = Just pkgTs}
        | any ((< pkgTs) . revisionTimestamp) packageVersionRevisions -> do
            error $
              unlines
                [ inputDir </> metaFile <> " has a revision with timestamp earlier than the package itself.",
                  "Adjust the timestamps so that all revisions come after the original package"
                ]
      meta ->
        return meta

  srcDir <- prepareSource pkgId pkgSpec

  let originalCabalFilePath = srcDir </> prettyShow pkgName <.> "cabal"
      cabalFileRevisionPath revisionNumber =
        joinPath
          [ inputDir,
            prettyShow pkgName,
            prettyShow pkgVersion,
            "revisions",
            show revisionNumber
          ]
          <.> "cabal"

  let cabalFilePath =
        maybe
          originalCabalFilePath
          cabalFileRevisionPath
          (latestRevisionNumber pkgSpec)

  pkgDesc <- readGenericPackageDescription' cabalFilePath

  sdistPath <- prepareSdist srcDir

  let expectedSdistName = prettyShow pkgId <.> "tar.gz"
  unless (takeFileName sdistPath == expectedSdistName) $ do
    error $
      unlines
        [ "creating a source distribution for " ++ prettyShow pkgId ++ " has failed because",
          "cabal has produced a source distribtion that does not match the expected file name:",
          "actual: " ++ takeBaseName sdistPath,
          "expected: " ++ expectedSdistName,
          "possible cause: the package name and/or version implied by the metadata file path does not match what is contained in the cabal file",
          "metadata file: " ++ metaFile,
          "version in cabal file: " ++ prettyShow (Distribution.Types.PackageId.pkgVersion $ package $ packageDescription pkgDesc)
        ]

  let cabalFileRevisions =
        sortOn
          (Down . fst)
          [ (revisionTimestamp, cabalFileRevisionPath revisionNumber)
            | RevisionSpec {revisionTimestamp, revisionNumber} <- packageVersionRevisions pkgSpec
          ]

  return
    PreparedPackageVersion
      { pkgId,
        pkgTimestamp = packageVersionTimestamp pkgSpec,
        pkgVersionSource = packageVersionSource pkgSpec,
        pkgVersionForce = packageVersionForce pkgSpec,
        pkgDesc,
        sdistPath,
        cabalFilePath,
        originalCabalFilePath,
        cabalFileRevisions
      }
