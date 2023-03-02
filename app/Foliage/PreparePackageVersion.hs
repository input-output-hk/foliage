{-# LANGUAGE PatternSynonyms #-}

module Foliage.PreparePackageVersion
  ( PreparedPackageVersion
      ( pkgId,
        pkgTimestamp,
        pkgVersionSource,
        pkgVersionForce,
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
import Distribution.Client.Compat.Prelude (prettyShow)
import Distribution.Types.PackageId
import Foliage.Meta (PackageVersionSource, PackageVersionSpec (..), RevisionSpec (..), UTCTime, latestRevisionNumber)
import Foliage.PrepareSdist (prepareSdist)
import Foliage.PrepareSource (prepareSource)
import System.FilePath (takeBaseName, takeFileName, (<.>), (</>))

data PreparedPackageVersion = PreparedPackageVersion
  { pkgId :: PackageId,
    pkgTimestamp :: Maybe UTCTime,
    pkgVersionSource :: PackageVersionSource,
    pkgVersionForce :: Bool,
    sdistPath :: FilePath,
    cabalFilePath :: FilePath,
    originalCabalFilePath :: FilePath,
    cabalFileRevisions :: [(UTCTime, FilePath)]
  }

preparePackageVersion :: FilePath -> PackageId -> PackageVersionSpec -> Action PreparedPackageVersion
preparePackageVersion inputDir pkgId pkgSpec = do
  srcDir <- prepareSource pkgId pkgSpec

  let PackageIdentifier pkgName pkgVersion = pkgId
      originalCabalFilePath = srcDir </> prettyShow pkgName <.> "cabal"
      cabalFileRevisionPath revisionNumber = inputDir </> prettyShow pkgName </> prettyShow pkgVersion </> "revisions" </> show revisionNumber <.> "cabal"

  let cabalFilePath =
        maybe
          originalCabalFilePath
          cabalFileRevisionPath
          (latestRevisionNumber pkgSpec)

  sdistPath <- prepareSdist srcDir

  let expectedSdistName = prettyShow pkgId <.> "tar.gz"
  unless (takeFileName sdistPath == expectedSdistName) $
    fail $
      unlines
        [ "creating a source distribution for " ++ prettyShow pkgId ++ " has failed because",
          "cabal has produced a source distribtion that does not match the expected file name:",
          "actual: " ++ takeBaseName sdistPath,
          "expected: " ++ expectedSdistName
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
        sdistPath,
        cabalFilePath,
        originalCabalFilePath,
        cabalFileRevisions
      }
