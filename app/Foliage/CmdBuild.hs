{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, void, when, (>=>))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Version
import Foliage.FetchURL (addFetchURLRule)
import Foliage.HackageSecurity hiding (ToJSON, toJSON)
import Foliage.Meta qualified as Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Oracles
import Foliage.Pages
import Foliage.TUF as TUF (
  mkMirrors,
  mkRoot,
  mkSnapshot,
  mkTimestamp,
 )

import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Traversable (for)
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Parsec (simpleParsec)
import Distribution.Utils.Generic (sndOf3)
import Distribution.Verbosity qualified as Verbosity
import Foliage.Meta (DeprecationSpec (..), PackageVersionSpec (..), RevisionSpec (..))
import Foliage.PrepareSdist (prepareSource)
import Foliage.Time qualified as Time
import Hackage.Security.Util.Path qualified as Sec
import System.Directory qualified as IO

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  let inputDir = buildOptsInputDir buildOptions
  let outputDir = buildOptsOutputDir buildOptions

  -- Create keys if needed
  case buildOptsSignOpts buildOptions of
    SignOptsSignWithKeys keysPath -> do
      ks <- IO.doesDirectoryExist keysPath
      unless ks $ do
        putStrLn $ "You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
        createKeys keysPath
    _otherwise -> pure ()

  shake opts $ do
    -- FIXME: consider using configuration variables (usingConfig/getConfig) or shakeExtra
    _ <- addOutputDirOracle (buildOptsOutputDir buildOptions)
    _ <- addInputDirOracle (buildOptsInputDir buildOptions)
    _ <- addCacheDirOracle cacheDir
    _ <- addExpiryTimeOracle (buildOptsExpireSignaturesOn buildOptions)
    _ <- addCurrentTimeOracle (buildOptsCurrentTime buildOptions)

    -- FIXME: use configuration variable or shakeExtra
    _ <- addSigninigKeysOracle (buildOptsSignOpts buildOptions)

    addFetchURLRule

    readPackageVersionSpec <- newCache $ \path -> do
      need [path]
      meta <- liftIO $ Meta.readPackageVersionSpec path
      validateMeta path meta
      return meta

    allPkgSpecs <- do
      cache <- newCache $ const $ do
        metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

        when (null metaFiles) $ do
          error $
            unlines
              [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)"
              , "Make sure you are passing the right input directory. The default input directory is _sources"
              ]

        forP metaFiles $ \path ->
          case filePattern "*/*/meta.toml" path of
            Just [name, version] -> do
              let pkgName = fromMaybe (error $ "invalid package name: " ++ name) $ simpleParsec name
              let pkgVersion = fromMaybe (error $ "invalid package version: " ++ version) $ simpleParsec version
              let pkgId = PackageIdentifier pkgName pkgVersion
              pkgSpec <- readPackageVersionSpec path
              return (inputDir </> path, pkgId, pkgSpec)
            _ -> error "the impossible happened"

      return $ cache ()

    ( (cacheDir </> "*/*.cabal") `matching` \case
        [pkgIdStr, pkgName']
          | Just pkgId <- simpleParsec pkgIdStr
          , unPackageName (pkgName pkgId) == pkgName' ->
              Just pkgId
        _ -> Nothing
      )
      ~~> \_path pkgId@PackageIdentifier{pkgName, pkgVersion} -> do
        let metaFile = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "meta.toml"
        pkgSpec <- readPackageVersionSpec metaFile
        void $ prepareSource metaFile pkgId pkgSpec

    --
    -- Core
    --

    outputDir </> "mirrors.json" %> TUF.mkMirrors
    outputDir </> "root.json" %> TUF.mkRoot
    outputDir </> "snapshot.json" %> TUF.mkSnapshot
    outputDir </> "timestamp.json" %> TUF.mkTimestamp
    outputDir </> "01-index.tar" %> \path ->
      allPkgSpecs >>= makeIndexEntries >>= liftIO . BL.writeFile path . Tar.write
    outputDir </> "01-index.tar.gz" %> \path ->
      allPkgSpecs >>= makeIndexEntries >>= liftIO . BL.writeFile path . GZip.compress . Tar.write

    ( (outputDir </> "package/*.tar.gz") `matching` \case
        [pkgId] -> simpleParsec pkgId
        _ -> Nothing
      )
      ~~> \path pkgId@PackageIdentifier{pkgName, pkgVersion} -> do
        let metaFile = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "meta.toml"
        pkgSpec <- readPackageVersionSpec metaFile
        pkgDesc <- prepareSource metaFile pkgId pkgSpec
        let srcDir = cacheDir </> prettyShow pkgId
        liftIO $ packageDirToSdist Verbosity.normal pkgDesc srcDir >>= BSL.writeFile path

    -- only require this when requested? the rule is always valid
    -- when (buildOptsWriteMetadata buildOptions) $
    outputDir </> "foliage/packages.json" %> \path ->
      allPkgSpecs >>= makeMetadataFile path

    --
    -- Pages
    --

    outputDir </> "index.html" %> makeIndexPage

    outputDir </> "all-packages/index.html" %> \path ->
      allPkgSpecs >>= makeAllPackagesPage path

    outputDir </> "all-package-versions/index.html" %> \path ->
      allPkgSpecs >>= makeAllPackageVersionsPage path

    ( (outputDir </> "package/*/index.html") `matching` \case
        [pkgId] -> simpleParsec pkgId
        _ -> Nothing
      )
      ~~> \path pkgId@PackageIdentifier{pkgName, pkgVersion} -> do
        let metaFile = inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "meta.toml"
        pkgSpec <- readPackageVersionSpec metaFile
        -- NOTE: this is a bit overshoot I think
        pkgDesc <- prepareSource metaFile pkgId pkgSpec
        makePackageVersionPage path pkgDesc pkgSpec
 where
  infixr 3 ~~>
  (~~>) :: (FilePath -> Maybe a) -> (FilePath -> a -> Action ()) -> Rules ()
  parser ~~> act = isJust . parser ?> \path -> act path $ fromJust $ parser path

  matching :: FilePattern -> ([String] -> Maybe a) -> FilePath -> Maybe a
  matching pattern parser = filePattern pattern >=> parser

  cacheDir = "_cache"
  opts =
    shakeOptions
      { shakeFiles = cacheDir
      , shakeChange = ChangeModtimeAndDigest
      , shakeColor = True
      , shakeLint = Just LintBasic
      , shakeReport = ["report.html", "report.json"]
      , shakeThreads = buildOptsNumThreads buildOptions
      , shakeVerbosity = buildOptsVerbosity buildOptions
      }

--       -- original cabal file, with its timestamp (if specified)
--       copyFileChanged originalCabalFilePath (outputDir </> "package" </> prettyShow pkgId </> "revision" </> "0" <.> "cabal")
--       -- all revised cabal files, with their timestamp
--       copyFileChanged cabalFilePath (outputDir </> "package" </> prettyShow pkgId </> "revision" </> show revNum <.> "cabal")
--       -- current version of the cabal file (after the revisions, if any)
--       copyFileChanged cabalFilePath (outputDir </> "package" </> prettyShow pkgId </> prettyShow (pkgName pkgId) <.> "cabal")

makeIndexEntries :: [(FilePath, PackageId, PackageVersionSpec)] -> Action [Tar.Entry]
makeIndexEntries allPkgSpecs = do
  cabalEntries <- makeCabalEntries allPkgSpecs
  metadataEntries <- makeMetadataEntries allPkgSpecs
  let extraEntries = makeExtraEntries allPkgSpecs

  -- WARN: See note on `makeCabalEntries`, the sorting here has to be stable
  return $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries ++ extraEntries)

makeCabalEntries :: [(FilePath, PackageId, PackageVersionSpec)] -> Action [Tar.Entry]
makeCabalEntries allPkgSpecs = do
  cacheDir <- askOracle CacheDir
  currentTime <- askOracle CurrentTime
  allPkgSpecs
    & foldMap
      ( \(metaFile, pkgId, pkgSpec) -> do
          let pkgCabalFile = cacheDir </> prettyShow pkgId </> unPackageName (pkgName pkgId) <.> "cabal"
              pkgTimestamp = fromMaybe currentTime (packageVersionTimestamp pkgSpec)

          uploadEntry <- makeIndexPkgCabal pkgId pkgTimestamp pkgCabalFile

          revisionEntries <- for (packageVersionRevisions pkgSpec) $ \RevisionSpec{revisionTimestamp, revisionNumber} -> do
            let cabalFileRevisionPath = takeDirectory metaFile </> "revisions" </> show revisionNumber <.> "cabal"
            makeIndexPkgCabal pkgId revisionTimestamp cabalFileRevisionPath

          -- WARN: So far Foliage allows publishing a package and a cabal file revision with the same timestamp
          -- This accidentally works because 1) the following inserts the original cabal file before the revisions
          -- AND 2) Data.List.sortOn is stable. The revised cabal file will always be after the original one.
          return $ uploadEntry : revisionEntries
      )

makeMetadataEntries :: [(FilePath, PackageId, PackageVersionSpec)] -> Action [Tar.Entry]
makeMetadataEntries allPkgSpecs = do
  currentTime <- askOracle CurrentTime
  expiryTime <- askOracle ExpiryTime
  outputDir <- askOracle OutputDir

  targetKeys <- readKeys "target"

  for allPkgSpecs $ \(_metaFile, pkgId, pkgSpec) -> do
    let sdistPath = outputDir </> "package" </> prettyShow pkgId <.> "tar.gz"
        pkgTimestamp = fromMaybe currentTime (packageVersionTimestamp pkgSpec)
    targets <- makeIndexPkgMetadata expiryTime pkgId sdistPath
    return $ mkTarEntry (renderSignedJSON targetKeys targets) (IndexPkgMetadata pkgId) pkgTimestamp

-- Currently `extraEntries` are only used for encoding `prefered-versions`.
makeExtraEntries :: [(FilePath, PackageId, PackageVersionSpec)] -> [Tar.Entry]
makeExtraEntries allPkgSpecs =
  let
    -- Group all (package) versions by package (name)
    groupedPackageVersions :: [NE.NonEmpty (FilePath, PackageId, PackageVersionSpec)]
    groupedPackageVersions = NE.groupWith sndOf3 allPkgSpecs

    -- All versions of a given package together form a list of entries
    -- The list of entries might be empty (in case no version has been deprecated)
    generateEntriesForGroup :: NE.NonEmpty (FilePath, PackageId, PackageVersionSpec) -> [Tar.Entry]
    generateEntriesForGroup packageGroup = map createTarEntry effectiveRanges
     where
      -- Get the package name of the current group.
      pn :: PackageName
      pn = packageName $ sndOf3 $ NE.head packageGroup
      -- Collect and sort the deprecation changes for the package group, turning them into a action on VersionRange
      deprecationChanges :: [(Meta.UTCTime, VersionRange -> VersionRange)]
      deprecationChanges =
        packageGroup
          & foldMap
            ( \(_metaFile, pkgId, pkgSpec) ->
                [ (deprecationTimestamp, rangeAct)
                | DeprecationSpec{deprecationTimestamp, deprecationIsDeprecated} <- packageVersionDeprecations pkgSpec
                , let rangeAct =
                        if deprecationIsDeprecated
                          then intersectVersionRanges (notThisVersion (pkgVersion pkgId))
                          else unionVersionRanges (thisVersion (pkgVersion pkgId))
                ]
            )

      -- Calculate (by applying them chronologically) the effective `VersionRange` for the package group.
      effectiveRanges :: [(Meta.UTCTime, VersionRange)]
      effectiveRanges = NE.tail $ NE.scanl applyChangeToRange (posixSecondsToUTCTime 0, anyVersion) (sortOn fst deprecationChanges)

      -- Apply a given change (`VersionRange -> VersionRange`) to a `VersionRange` and
      -- return the simplified the result with a new timestamp.
      applyChangeToRange
        :: (Meta.UTCTime, VersionRange)
        -> (Meta.UTCTime, VersionRange -> VersionRange)
        -> (Meta.UTCTime, VersionRange)
      applyChangeToRange (_, range) (ts, change) = (ts, simplifyVersionRange $ change range)

      -- Create a `Tar.Entry` for the package group, its computed `VersionRange` and a timestamp.
      createTarEntry (ts, effectiveRange) = mkTarEntry (BL.pack $ prettyShow dep) (IndexPkgPrefs pn) ts
       where
        -- Cabal uses `Dependency` to represent preferred versions, cf.
        -- `parsePreferredVersions`. The (sub)libraries part is ignored.
        dep = mkDependency pn effectiveRange mainLibSet
   in
    foldMap generateEntriesForGroup groupedPackageVersions

makeMetadataFile :: FilePath -> [(FilePath, PackageId, PackageVersionSpec)] -> Action ()
makeMetadataFile path allPkgSpecs = do
  liftIO $ Aeson.encodeFile path metadata
 where
  metadata =
    map
      ( \(_, pkgId, pkgSpec) ->
          Aeson.object $
            [ "pkg-name" Aeson..= pkgName pkgId
            , "pkg-version" Aeson..= pkgVersion pkgId
            , "url" Aeson..= Meta.packageVersionSourceToUri (packageVersionSource pkgSpec)
            ]
              ++ ["forced-version" Aeson..= True | packageVersionForce pkgSpec]
              ++ (case packageVersionTimestamp pkgSpec of Nothing -> []; Just t -> ["timestamp" Aeson..= t])
      )
      allPkgSpecs

makeIndexPkgCabal :: PackageId -> Meta.UTCTime -> FilePath -> Action Tar.Entry
makeIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BS.readFile filePath
  pure $ mkTarEntry (BL.fromStrict contents) (IndexPkgCabal pkgId) timestamp

makeIndexPkgMetadata :: Maybe Meta.UTCTime -> PackageId -> FilePath -> Action Targets
makeIndexPkgMetadata expiryTime pkgId sdistPath = do
  need [sdistPath]
  targetFileInfo <- liftIO $ computeFileInfoSimple' sdistPath
  let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
  return
    Targets
      { targetsVersion = FileVersion 1
      , targetsExpires = FileExpires expiryTime
      , targetsTargets = fromList [(TargetPathRepo packagePath, targetFileInfo)]
      , targetsDelegations = Nothing
      }

duplicates :: (Ord a) => NE.NonEmpty a -> [a]
duplicates = mapMaybe (listToMaybe . NE.tail) . NE.group

doubleDeprecations :: NE.NonEmpty DeprecationSpec -> [NE.NonEmpty DeprecationSpec]
doubleDeprecations = filter ((> 1) . length) . NE.groupWith deprecationIsDeprecated

-- | TEMP
validateMeta :: (MonadFail m) => FilePath -> PackageVersionSpec -> m ()
validateMeta metaFile PackageVersionSpec{..} = do
  case (NE.nonEmpty packageVersionRevisions, packageVersionTimestamp) of
    (Just _someRevisions, Nothing) ->
      fail $
        unlines
          [ metaFile <> " has cabal file revisions but the package has no timestamp."
          , "This combination doesn't make sense. Either add a timestamp on the original package or remove the revisions."
          ]
    (Just (NE.sort -> someRevisions), Just ts)
      -- WARN: this should really be a <=
      | revisionTimestamp (NE.head someRevisions) < ts ->
          fail $
            unlines
              [ metaFile <> " has a revision with timestamp earlier than the package itself."
              , "Adjust the timestamps so that all revisions come after the package publication."
              ]
      | not (null $ duplicates (revisionTimestamp <$> someRevisions)) ->
          fail $
            unlines
              [ metaFile <> " has two revisions entries with the same timestamp."
              , "Adjust the timestamps so that all the revisions happen at a different time."
              ]
    _otherwise ->
      return ()

  case (NE.nonEmpty packageVersionDeprecations, packageVersionTimestamp) of
    (Just _someDeprecations, Nothing) ->
      fail $
        unlines
          [ metaFile <> " has deprecations but the package has no timestamp."
          , "This combination doesn't make sense. Either add a timestamp on the original package or remove the deprecation."
          ]
    (Just (NE.sort -> someDeprecations), Just ts)
      | deprecationTimestamp (NE.head someDeprecations) <= ts ->
          fail $
            unlines
              [ metaFile <> " has a deprecation entry with timestamp earlier (or equal) than the package itself."
              , "Adjust the timestamps so that all the (un-)deprecations come after the package publication."
              ]
      | not (deprecationIsDeprecated (NE.head someDeprecations)) ->
          fail $
            "The first deprecation entry in" <> metaFile <> " cannot be an un-deprecation"
      | not (null $ duplicates (deprecationTimestamp <$> someDeprecations)) ->
          fail $
            unlines
              [ metaFile <> " has two deprecation entries with the same timestamp."
              , "Adjust the timestamps so that all the (un-)deprecations happen at a different time."
              ]
      | not (null $ doubleDeprecations someDeprecations) ->
          fail $
            unlines
              [ metaFile <> " contains two consecutive deprecations or two consecutive un-deprecations."
              , "Make sure deprecations and un-deprecations alternate in time."
              ]
    _otherwise ->
      return ()

mkTarEntry
  :: BL.ByteString
  -> IndexFile dec
  -> Meta.UTCTime
  -> Tar.Entry
mkTarEntry contents indexFile timestamp =
  (Tar.fileEntry tarPath contents)
    { Tar.entryTime = floor $ Time.utcTimeToPOSIXSeconds timestamp
    , Tar.entryOwnership =
        Tar.Ownership
          { Tar.ownerName = "foliage"
          , Tar.groupName = "foliage"
          , Tar.ownerId = 0
          , Tar.groupId = 0
          }
    }
 where
  tarPath = case Tar.toTarPath False indexPath of
    Left e -> error $ "Invalid tar path " ++ indexPath ++ "(" ++ e ++ ")"
    Right tp -> tp

  indexPath = Sec.toFilePath $ Sec.castRoot $ indexFileToPath hackageIndexLayout indexFile
