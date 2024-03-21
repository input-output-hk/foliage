{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Foliage.PreparePackageVersion (
  PreparedPackageVersion (
    pkgId,
    pkgTimestamp,
    pkgVersionSource,
    pkgVersionForce,
    pkgVersionIsDeprecated,
    pkgVersionDeprecationChanges,
    pkgDesc,
    sdistPath,
    cabalFilePath,
    originalCabalFilePath,
    cabalFileRevisions
  ),
  pattern PreparedPackageVersion,
  preparePackageVersion,
  Timestamped (..),
)
where

import Control.Monad (unless)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Foldable (foldlM)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Development.Shake (Action, CmdOption (..), Stdout (..), cmd, copyFileChanged, liftIO, need)
import Development.Shake.FilePath (joinPath, splitDirectories)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (packageDescription))
import Distribution.Types.PackageDescription (PackageDescription (package))
import Distribution.Types.PackageId
import Foliage.Meta (DeprecationSpec (..), PackageVersionSource, PackageVersionSpec (..), RevisionSpec (..), UTCTime, latestRevisionNumber)
import Foliage.PrepareSdist (prepareSdist)
import Foliage.PrepareSource (prepareSource)
import Foliage.Shake (readGenericPackageDescription', readPackageVersionSpec')
import System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))

data Timestamped a = Timestamped {timestamp :: UTCTime, timestampedValue :: a}
  deriving (Eq, Ord, Show)

-- TODO: can we ensure that `pkgVersionDeprecationChanges` and `cabalFileRevisions` are
-- sorted by timestamp? e.g https://hackage.haskell.org/package/sorted-list
data PreparedPackageVersion = PreparedPackageVersion
  { pkgId :: PackageId
  , pkgTimestamp :: Maybe UTCTime
  , pkgVersionSource :: PackageVersionSource
  , pkgVersionForce :: Bool
  , pkgVersionIsDeprecated :: Bool
  , pkgVersionDeprecationChanges :: [(UTCTime, Bool)]
  , pkgDesc :: GenericPackageDescription
  , sdistPath :: FilePath
  , -- latest cabal file
    cabalFilePath :: FilePath
  , -- cabal file in the sdist
    originalCabalFilePath :: FilePath
  , -- all cabal file revisions
    cabalFileRevisions :: [Timestamped FilePath]
  }

-- @andreabedini comments:
--
-- The function `preparePackageVersion` has a bit of a special role which I
-- should comment upon.
--
-- There are at three sources of information about a package:
--
--   * the path of the meta file: `_sources/pkg-name/pkg-version/meta.toml`
--   * the content of `meta.toml`
--   * the tarball/sdist pointed by `meta.toml`
--
--
-- Before #37 I used to refer to these three pieces of data independently,
-- thinking it would be a good idea to keep the data-pipeline granular.
--
-- While working on #37, I realised this granularity was leading me to have
-- consistency checks scattered around the code so I figured it would make more
-- sense to centralise these checks into a single function and to use a type
-- (`PreparedPackageVersion`) as evidence that everything is consistent (e.g.
-- the package name inferred from the meta.toml path is the same as the one in
-- the cabal file of the source distribution).
--
-- This function has also the chance to denormalise some data (i.e. repeating it
-- multiple times in different forms) for easy consumption downstream. This
-- could be split out in the future if `PreparedPackageVersion` starts to become
-- a kitchen sink.

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
      PackageVersionSpec{packageVersionRevisions, packageVersionTimestamp = Nothing}
        | not (null packageVersionRevisions) -> do
            error $
              unlines
                [ inputDir </> metaFile <> " has cabal file revisions but the original package has no timestamp."
                , "This combination doesn't make sense. Either add a timestamp on the original package or remove the revisions"
                ]
      PackageVersionSpec{packageVersionRevisions, packageVersionTimestamp = Just pkgTs}
        | any ((< pkgTs) . revisionTimestamp) packageVersionRevisions -> do
            error $
              unlines
                [ inputDir </> metaFile <> " has a revision with timestamp earlier than the package itself."
                , "Adjust the timestamps so that all revisions come after the original package"
                ]
      meta ->
        return meta

  srcDir <- prepareSource pkgId pkgSpec

  -- FIXME: This produce a Shake error since it `need` the file:
  --
  --     revisionNumber <.> "cabal"
  --
  -- ... which could now be a `.diff` or a `.patch`!
  --
  -- @andreabedini commented:
  --
  -- > I don't think that cabalFileRevisions :: [Timestamped FilePath] can work
  -- > anymore since there's no filepath for a computed revision (unless we put
  -- > it in _cache but I would avoid that).
  -- >
  -- > @yvan-sraka I think the correct solution is to turn cabalFileRevisions
  -- > into [Timestamped ByteString] and compute the revisions as part of

  let originalCabalFilePath = srcDir </> prettyShow pkgName <.> "cabal"

      cabalFileRevisionPath revisionNumber =
        joinPath
          [ inputDir
          , prettyShow pkgName
          , prettyShow pkgVersion
          , "revisions"
          , show revisionNumber
          ]
          <.> "cabal"

      cabalFilePath =
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
        [ "creating a source distribution for " ++ prettyShow pkgId ++ " has failed because"
        , "cabal has produced a source distribtion that does not match the expected file name:"
        , "actual: " ++ takeBaseName sdistPath
        , "expected: " ++ expectedSdistName
        , "possible cause: the package name and/or version implied by the metadata file path does not match what is contained in the cabal file"
        , "metadata file: " ++ metaFile
        , "version in cabal file: " ++ prettyShow (Distribution.Types.PackageId.pkgVersion $ package $ packageDescription pkgDesc)
        ]

  let pkgVersionDeprecationChanges =
        sortOn
          Down
          [ (deprecationTimestamp, deprecationIsDeprecated)
          | DeprecationSpec{deprecationTimestamp, deprecationIsDeprecated} <- packageVersionDeprecations pkgSpec
          ]

  let pkgVersionIsDeprecated = maybe False snd $ listToMaybe pkgVersionDeprecationChanges

  -- use contents rather than path
  cabalFileRevisions <-
    sortOn
      (Down . timestamp)
      <$> foldlM
        ( \prevCabalFilePath (RevisionSpec{revisionTimestamp, revisionNumber}) -> do
            let inputFile = cabalFileRevisionPath revisionNumber
            let (Timestamped patch _) = prevCabalFilePath
            if takeExtension inputFile `elem` [".diff", ".patch"]
              then do
                Stdout out <- cmd ["patch", "-i", inputFile, "-o", "-", prevCabalFilePath]
                return $ Timestamped revisionTimestamp out
              else Timestamped revisionTimestamp <$> BL.readFile inputFile
        )
        originalCabalFilePath
        (packageVersionRevisions pkgSpec)

  return
    PreparedPackageVersion
      { pkgId
      , pkgTimestamp = packageVersionTimestamp pkgSpec
      , pkgVersionSource = packageVersionSource pkgSpec
      , pkgVersionForce = packageVersionForce pkgSpec
      , pkgVersionDeprecationChanges
      , pkgVersionIsDeprecated
      , pkgDesc
      , sdistPath
      , cabalFilePath
      , originalCabalFilePath
      , cabalFileRevisions
      }
