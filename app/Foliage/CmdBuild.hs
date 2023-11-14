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
import Codec.Archive.Tar.Entry.Orphans ()
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, when, (>=>))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Version
import Foliage.FetchURL (addFetchURLRule)
import Foliage.HackageSecurity (
  computeFileInfoSimple,
  createKeys,
  mkMirrors,
  mkRoot,
  mkSnapshot,
  mkTimestamp,
  readKeys,
  renderSignedJSON,
 )
import Foliage.Meta qualified as Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Oracles
import Foliage.Pages
import Hackage.Security.Server (
  FileExpires (..),
  FileVersion (..),
  IndexFile (..),
  IndexLayout (..),
  RepoLayout (..),
  TargetPath (..),
  Targets (..),
  hackageIndexLayout,
  hackageRepoLayout,
 )

import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Traversable (for)
import Development.Shake.Classes
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Utils.Generic (sndOf3)
import Distribution.Verbosity qualified as Verbosity
import Foliage.Meta (DeprecationSpec (..), PackageVersionSpec (..), RevisionSpec (..))
import Foliage.SourceDist (applyPatches, fetchPackageVersion, updateCabalFileVersion)
import Foliage.Time (truncateSeconds)
import Hackage.Security.TUF.FileMap qualified as FM
import Hackage.Security.Util.Path qualified as Sec
import System.Directory qualified as IO
import Text.Read (readMaybe)

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  let
    inputDir = buildOptsInputDir buildOptions
    outputDir = buildOptsOutputDir buildOptions
    cacheDir = "_cache"

  currentTime <- case buildOptsCurrentTime buildOptions of
    Nothing -> do
      t <- truncateSeconds <$> liftIO getCurrentTime
      putStrLn $ "Current time set to " <> iso8601Show t <> ". You can set a fixed time using the --current-time option."
      return t
    Just t -> do
      putStrLn $ "Current time set to " <> iso8601Show t <> "."
      return t

  let expireSignaturesOn = buildOptsExpireSignaturesOn buildOptions
  for_ expireSignaturesOn $ \time ->
    putStrLn $ "Expiry time set to " <> iso8601Show time

  let
    extra =
      ( case buildOptsSignOpts buildOptions of
          SignOptsSignWithKeys keys -> addShakeExtra (SignWithKeys keys)
          SignOptsDon'tSign -> id
      )
        . addShakeExtra (FileExpires expireSignaturesOn)
        $ mempty

  let
    opts =
      shakeOptions
        { shakeFiles = cacheDir
        , shakeChange = ChangeModtimeAndDigest
        , shakeColor = True
        , shakeLint = Just LintBasic
        , shakeReport = ["report.html", "report.json"]
        , shakeThreads = buildOptsNumThreads buildOptions
        , shakeVerbosity = buildOptsVerbosity buildOptions
        , shakeExtra = extra
        }

  -- Create keys if needed
  case buildOptsSignOpts buildOptions of
    SignOptsSignWithKeys keysPath -> do
      ks <- IO.doesDirectoryExist keysPath
      unless ks $ do
        putStrLn $ "You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
        createKeys keysPath
    _otherwise -> pure ()

  shake opts $ do
    _ <- addCacheDirOracle cacheDir

    addFetchURLRule

    readPackageVersionSpec <- newCache $ \path -> do
      need [path]
      meta <- liftIO $ Meta.readPackageVersionSpec path
      validateMeta path meta
      return meta

    action $ do
      need
        [ outputDir </> "mirrors.json"
        , outputDir </> "root.json"
        , outputDir </> "snapshot.json"
        , outputDir </> "timestamp.json"
        ]

    let metaFileForPkgId :: PackageIdentifier -> FilePath
        metaFileForPkgId PackageIdentifier{pkgName, pkgVersion} =
          inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "meta.toml"

    let cabalFileForPkgId :: PackageIdentifier -> FilePath
        cabalFileForPkgId PackageIdentifier{pkgName, pkgVersion} =
          cacheDir </> unPackageName pkgName </> prettyShow pkgVersion </> unPackageName pkgName <.> "cabal"

    let cabalFileRevisionForPkgId :: PackageIdentifier -> Int -> FilePath
        cabalFileRevisionForPkgId pkgId revNum =
          metaFileForPkgId pkgId `replaceBaseName` "revisions" </> show revNum <.> "cabal"

    let sdistPathForPkgId :: PackageIdentifier -> FilePath
        sdistPathForPkgId pkgId =
          outputDir </> "package" </> prettyShow pkgId <.> "tar.gz"

    --
    --
    --

    cacheDir </> "*/*/*.cabal"
      ~?~ ( \case
              [name, version, name']
                | Just pkgName <- simpleParsec name
                , Just pkgVersion <- simpleParsec version
                , name == name' ->
                    Just $ PackageIdentifier pkgName pkgVersion
              _ -> Nothing
          )
      ~~> \cabalFilePath pkgId -> do
        let metaFile = metaFileForPkgId pkgId
        pkgSpec <- readPackageVersionSpec metaFile
        let PackageIdentifier{pkgVersion} = pkgId
            PackageVersionSpec{packageVersionSource, packageVersionForce} = pkgSpec

        fetchPackageVersion packageVersionSource (takeDirectory cabalFilePath)

        applyPatches metaFile (takeDirectory cabalFilePath)

        when packageVersionForce $ do
          updateCabalFileVersion cabalFilePath pkgVersion
    --
    -- Index creation
    --

    getPkgSpecs <- do
      getPkgSpecs' <- addOracleCache $ \PkgSpecs{} -> do
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
              pkgSpec <- readPackageVersionSpec (inputDir </> path)
              return (inputDir </> path, pkgId, pkgSpec)
            _ -> error "the impossible happened"
      return $ getPkgSpecs' $ PkgSpecs ()

    indexEntries <- do
      getIndexEntries <- newCache $ \IndexEntries{} -> do
        pkgSpecs <- getPkgSpecs
        cabalEntries <- makeCabalEntries currentTime cabalFileForPkgId pkgSpecs
        metadataEntries <- makeMetadataEntries currentTime sdistPathForPkgId pkgSpecs
        let extraEntries = makeExtraEntries pkgSpecs

        -- WARN: See note on `makeCabalEntries`, the sorting here has to be stable
        return $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries ++ extraEntries)
      -- Return the oracle function applied to IndexEntries ()
      return $ getIndexEntries $ IndexEntries ()

    --
    -- Rules for core repository functionality
    --
    coreRules outputDir cabalFileForPkgId indexEntries

    alternatives $ do
      outputDir </> "package/*/revision/0.cabal"
        ~?~ ( \case
                [pkgId] -> simpleParsec pkgId
                _ -> Nothing
            )
        ~~> \path pkgId ->
          copyFileChanged (cabalFileForPkgId pkgId) path

      outputDir </> "package/*/revision/*.cabal"
        ~?~ ( \case
                [pkgId, revNum] -> (,) <$> simpleParsec pkgId <*> readMaybe revNum
                _ -> Nothing
            )
        ~~> \path (pkgId, revNum) ->
          copyFileChanged (cabalFileRevisionForPkgId pkgId revNum) path

    outputDir </> "package/*/*.cabal"
      ~?~ ( \case
              [pkgIdStr, pkgName']
                | Just pkgId <- simpleParsec pkgIdStr
                , unPackageName (pkgName pkgId) == pkgName' ->
                    Just pkgId
              _ -> Nothing
          )
      ~~> \path pkgId ->
        copyFileChanged (cabalFileForPkgId pkgId) path

    --
    -- Foliage metadata
    --

    -- only require this when requested? the rule is always valid
    -- when (buildOptsWriteMetadata buildOptions) $
    outputDir </> "foliage/packages.json"
      %> \path ->
        getPkgSpecs >>= makeMetadataFile path

    --
    -- Pages
    --

    outputDir </> "index.html"
      %> makeIndexPage

    outputDir </> "all-packages/index.html"
      %> \path ->
        getPkgSpecs >>= makeAllPackagesPage currentTime path

    outputDir </> "all-package-versions/index.html"
      %> \path ->
        getPkgSpecs >>= makeAllPackageVersionsPage currentTime path

    outputDir </> "package/*/index.html"
      ~?~ ( \case
              [pkgId] -> simpleParsec pkgId
              _ -> Nothing
          )
      ~~> \path pkgId -> do
        let metaFile = metaFileForPkgId pkgId
        pkgSpec <- readPackageVersionSpec metaFile
        let cabalFile = cabalFileForPkgId pkgId
        need [cabalFile]
        pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.silent cabalFile
        makePackageVersionPage path pkgDesc pkgSpec

coreRules :: FilePath -> (PackageIdentifier -> FilePath) -> Action [Tar.Entry] -> Rules ()
coreRules outputDir cabalFileForPkgId indexEntries = do
  action $ do
    need
      [ outputDir </> "mirrors.json"
      , outputDir </> "root.json"
      , outputDir </> "snapshot.json"
      , outputDir </> "timestamp.json"
      ]

  outputDir </> "mirrors.json"
    %> mkMirrors

  outputDir </> "root.json"
    %> mkRoot

  outputDir </> "snapshot.json"
    %> mkSnapshot outputDir

  outputDir </> "timestmap.json"
    %> mkTimestamp outputDir

  outputDir </> "01-index.tar"
    %> \path ->
      indexEntries >>= liftIO . BL.writeFile path . Tar.write

  outputDir </> "01-index.tar.gz"
    %> \path ->
      indexEntries >>= liftIO . BL.writeFile path . GZip.compress . Tar.write

  outputDir </> "package/*.tar.gz"
    ~?~ ( \case
            [pkgId] -> simpleParsec pkgId
            _ -> Nothing
        )
    ~~> \path pkgId -> do
      let cabalFilePath = cabalFileForPkgId pkgId
      need [cabalFilePath]
      pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.silent cabalFilePath
      liftIO $ packageDirToSdist Verbosity.normal pkgDesc (takeDirectory cabalFilePath) >>= BSL.writeFile path

makeCabalEntries :: UTCTime -> (PackageId -> FilePath) -> [(FilePath, PackageId, PackageVersionSpec)] -> Action [Tar.Entry]
makeCabalEntries currentTime cabalFileForPkgId allPkgSpecs = do
  fmap concat $ forP allPkgSpecs $ \(metaFile, pkgId, pkgSpec) -> do
    let pkgCabalFile = cabalFileForPkgId pkgId
        pkgTimestamp = fromMaybe currentTime (packageVersionTimestamp pkgSpec)

    uploadEntry <- makeIndexPkgCabal pkgId pkgTimestamp pkgCabalFile

    revisionEntries <- for (packageVersionRevisions pkgSpec) $ \RevisionSpec{revisionTimestamp, revisionNumber} -> do
      let cabalFileRevisionPath = takeDirectory metaFile </> "revisions" </> show revisionNumber <.> "cabal"
      makeIndexPkgCabal pkgId revisionTimestamp cabalFileRevisionPath

    -- WARN: So far Foliage allows publishing a package and a cabal file revision with the same timestamp
    -- This accidentally works because 1) the following inserts the original cabal file before the revisions
    -- AND 2) Data.List.sortOn is stable. The revised cabal file will always be after the original one.
    return $ uploadEntry : revisionEntries

makeMetadataEntries :: UTCTime -> (PackageId -> FilePath) -> [(FilePath, PackageId, PackageVersionSpec)] -> Action [Tar.Entry]
makeMetadataEntries currentTime sdistPathForPkgId allPkgSpecs = do
  Just expiryTime <- getShakeExtra

  targetKeys <- readKeys "target"

  forP allPkgSpecs $ \(_metaFile, pkgId, pkgSpec) -> do
    let sdistPath = sdistPathForPkgId pkgId
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

makeIndexPkgMetadata :: FileExpires -> PackageId -> FilePath -> Action Targets
makeIndexPkgMetadata expiryTime pkgId sdistPath = do
  targetFileInfo <- computeFileInfoSimple sdistPath
  let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
  return
    Targets
      { targetsVersion = FileVersion 1
      , targetsExpires = expiryTime
      , targetsTargets = FM.fromList [(TargetPathRepo packagePath, targetFileInfo)]
      , targetsDelegations = Nothing
      }

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
 where
  duplicates :: (Ord a) => NE.NonEmpty a -> [a]
  duplicates = mapMaybe (listToMaybe . NE.tail) . NE.group

  doubleDeprecations :: NE.NonEmpty DeprecationSpec -> [NE.NonEmpty DeprecationSpec]
  doubleDeprecations = filter ((> 1) . length) . NE.groupWith deprecationIsDeprecated

mkTarEntry
  :: BL.ByteString
  -> IndexFile dec
  -> Meta.UTCTime
  -> Tar.Entry
mkTarEntry contents indexFile timestamp =
  (Tar.fileEntry tarPath contents)
    { Tar.entryTime = floor $ utcTimeToPOSIXSeconds timestamp
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

newtype IndexEntries = IndexEntries () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult IndexEntries = [Tar.Entry]

newtype PkgSpecs = PkgSpecs () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult PkgSpecs = [(FilePath, PackageId, PackageVersionSpec)]

newtype SignWithKeys = SignWithKeys FilePath

infixr 4 ~?~
(~?~) :: FilePattern -> ([String] -> Maybe c) -> FilePath -> Maybe c
pat ~?~ parser = filePattern pat >=> parser

infixr 3 ~~>
(~~>) :: (FilePath -> Maybe a) -> (FilePath -> a -> Action ()) -> Rules ()
parser ~~> act = isJust . parser ?> \path -> act path $ fromJust $ parser path
