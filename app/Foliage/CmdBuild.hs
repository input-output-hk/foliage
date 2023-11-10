{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, void, when)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Version
import Foliage.FetchURL (addFetchURLRule)
import Foliage.HackageSecurity hiding (ToJSON, toJSON)
import Foliage.Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Oracles
import Foliage.Pages
import Foliage.PreparePackageVersion (PreparedPackageVersion (..), preparePackageVersion)

-- import Foliage.PrepareSdist (addPrepareSdistRule)

import Development.Shake.Classes
import Foliage.Time qualified as Time
import GHC.Generics (Generic)
import Hackage.Security.Util.Path qualified as Sec
import System.Directory (createDirectoryIfMissing)
import System.Directory qualified as IO

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  let inputDir = buildOptsInputDir buildOptions
  let outputDir = buildOptsOutputDir buildOptions
  shake opts $ do
    _ <- addOutputDirOracle (buildOptsOutputDir buildOptions)
    _ <- addInputDirOracle (buildOptsInputDir buildOptions)
    _ <- addCacheDirOracle cacheDir
    _ <- addExpiryTimeOracle (buildOptsExpireSignaturesOn buildOptions)
    _ <- addCurrentTimeOracle (buildOptsCurrentTime buildOptions)

    -- Create keys if needed
    liftIO $ case buildOptsSignOpts buildOptions of
      SignOptsSignWithKeys keysPath -> do
        ks <- IO.doesDirectoryExist keysPath
        unless ks $ do
          putStrLn $ "You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
          createKeys keysPath
      _otherwise -> pure ()
    _ <- addSigninigKeysOracle (buildOptsSignOpts buildOptions)

    tufRules outputDir

    outputDir </> "01-index.tar" %> \path ->
      askOracle TarContents >>= liftIO . BL.writeFile path

    outputDir </> "01-index.tar.gz" %> \path ->
      askOracle TarContents >>= liftIO . BL.writeFile path . GZip.compress

    addFetchURLRule
    -- addPrepareSdistRule
    phony "buildAction" (buildAction buildOptions)
    want ["buildAction"]
 where
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

tufRules :: FilePath -> Rules ()
tufRules outputDir = do
  outputDir </> "mirrors.json" %> \path -> do
    expiryTime <- askOracle ExpiryTime
    privateKeysMirrors <- readKeys "mirrors"
    liftIO $
      writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysMirrors $
        Mirrors
          { mirrorsVersion = FileVersion 1
          , mirrorsExpires = FileExpires expiryTime
          , mirrorsMirrors = []
          }

  outputDir </> "root.json" %> \path -> do
    expiryTime <- askOracle ExpiryTime

    privateKeysRoot <- readKeys "root"
    privateKeysTarget <- readKeys "target"
    privateKeysSnapshot <- readKeys "snapshot"
    privateKeysTimestamp <- readKeys "timestamp"
    privateKeysMirrors <- readKeys "mirrors"

    liftIO $
      writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysRoot $
        Root
          { rootVersion = FileVersion 1
          , rootExpires = FileExpires expiryTime
          , rootKeys =
              fromKeys $
                concat
                  [ privateKeysRoot
                  , privateKeysTarget
                  , privateKeysSnapshot
                  , privateKeysTimestamp
                  , privateKeysMirrors
                  ]
          , rootRoles =
              RootRoles
                { rootRolesRoot =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysRoot
                      , roleSpecThreshold = KeyThreshold 2
                      }
                , rootRolesSnapshot =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysSnapshot
                      , roleSpecThreshold = KeyThreshold 1
                      }
                , rootRolesTargets =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysTarget
                      , roleSpecThreshold = KeyThreshold 1
                      }
                , rootRolesTimestamp =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysTimestamp
                      , roleSpecThreshold = KeyThreshold 1
                      }
                , rootRolesMirrors =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysMirrors
                      , roleSpecThreshold = KeyThreshold 1
                      }
                }
          }

  "snapshot.json" %> \path -> do
    expiryTime <- askOracle ExpiryTime
    privateKeysSnapshot <- readKeys "snapshot"

    rootInfo <- anchorRepoPath' repoLayoutRoot >>= computeFileInfoSimple
    mirrorsInfo <- anchorRepoPath' repoLayoutMirrors >>= computeFileInfoSimple
    tarInfo <- anchorRepoPath' repoLayoutIndexTar >>= computeFileInfoSimple
    tarGzInfo <- anchorRepoPath' repoLayoutIndexTarGz >>= computeFileInfoSimple

    liftIO $
      writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysSnapshot $
        Snapshot
          { snapshotVersion = FileVersion 1
          , snapshotExpires = FileExpires expiryTime
          , snapshotInfoRoot = rootInfo
          , snapshotInfoMirrors = mirrorsInfo
          , snapshotInfoTar = Just tarInfo
          , snapshotInfoTarGz = tarGzInfo
          }

  "timestamp.json" %> \path -> do
    expiryTime <- askOracle ExpiryTime
    privateKeysTimestamp <- readKeys "timestamp"

    snapshotInfo <- anchorRepoPath' repoLayoutSnapshot >>= computeFileInfoSimple

    liftIO $
      writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysTimestamp $
        Timestamp
          { timestampVersion = FileVersion 1
          , timestampExpires = FileExpires expiryTime
          , timestampInfoSnapshot = snapshotInfo
          }

buildAction :: BuildOptions -> Action ()
buildAction
  BuildOptions{buildOptsWriteMetadata = doWritePackageMeta} = do
    packageVersions <- getPackageVersions

    makeIndexPage

    makeAllPackagesPage packageVersions

    makeAllPackageVersionsPage packageVersions

    void $ forP packageVersions makePackageVersionPage

    when doWritePackageMeta $
      makeMetadataFile packageVersions

    cabalEntries <-
      foldMap
        ( \PreparedPackageVersion{pkgId, pkgTimestamp, cabalFilePath, originalCabalFilePath, cabalFileRevisions} -> do
            outputDir <- askOracle OutputDir
            currentTime <- askOracle CurrentTime
            -- original cabal file, with its timestamp (if specified)
            copyFileChanged originalCabalFilePath (outputDir </> "package" </> prettyShow pkgId </> "revision" </> "0" <.> "cabal")
            cf <- prepareIndexPkgCabal pkgId (fromMaybe currentTime pkgTimestamp) originalCabalFilePath

            -- all revised cabal files, with their timestamp
            revcf <- for (zip [1 :: Int ..] cabalFileRevisions) $ \(revNum, (timestamp, path)) -> do
              copyFileChanged cabalFilePath (outputDir </> "package" </> prettyShow pkgId </> "revision" </> show revNum <.> "cabal")
              prepareIndexPkgCabal pkgId timestamp path

            -- current version of the cabal file (after the revisions, if any)
            copyFileChanged cabalFilePath (outputDir </> "package" </> prettyShow pkgId </> prettyShow (pkgName pkgId) <.> "cabal")

            -- WARN: So far Foliage allows publishing a package and a cabal file revision with the same timestamp
            -- This accidentally works because 1) the following inserts the original cabal file before the revisions
            -- AND 2) Data.List.sortOn is stable. The revised cabal file will always be after the original one.
            return $ cf : revcf
        )
        packageVersions

    targetKeys <- readKeys "target"

    metadataEntries <-
      forP packageVersions $ \ppv@PreparedPackageVersion{pkgId, pkgTimestamp} -> do
        expiryTime <- askOracle ExpiryTime
        currentTime <- askOracle CurrentTime
        targets <- prepareIndexPkgMetadata expiryTime ppv
        pure $
          mkTarEntry
            (renderSignedJSON targetKeys targets)
            (IndexPkgMetadata pkgId)
            (fromMaybe currentTime pkgTimestamp)

    let extraEntries = getExtraEntries packageVersions

    -- WARN: See note above, the sorting here has to be stable
    let tarContents = Tar.write $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries ++ extraEntries)

    _

makeMetadataFile :: [PreparedPackageVersion] -> Action ()
makeMetadataFile packageVersions = do
  outputDir <- askOracle OutputDir
  traced "writing metadata" $ do
    createDirectoryIfMissing True (outputDir </> "foliage")
    Aeson.encodeFile
      (outputDir </> "foliage" </> "packages.json")
      (map encodePackageVersion packageVersions)
 where
  encodePackageVersion
    PreparedPackageVersion
      { pkgId = PackageIdentifier{pkgName, pkgVersion}
      , pkgTimestamp
      , pkgVersionForce
      , pkgVersionSource
      } =
      Aeson.object
        ( [ "pkg-name" Aeson..= pkgName
          , "pkg-version" Aeson..= pkgVersion
          , "url" Aeson..= packageVersionSourceToUri pkgVersionSource
          ]
            ++ ["forced-version" Aeson..= True | pkgVersionForce]
            ++ (case pkgTimestamp of Nothing -> []; Just t -> ["timestamp" Aeson..= t])
        )

getPackageVersions :: Action [PreparedPackageVersion]
getPackageVersions = do
  inputDir <- askOracle InputDir
  metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

  when (null metaFiles) $ do
    error $
      unlines
        [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)"
        , "Make sure you are passing the right input directory. The default input directory is _sources"
        ]

  -- Only pass the directory and as a InputPath
  let metaFiles' = map (Sec.Path @InputRoot . takeDirectory) metaFiles
  forP metaFiles' preparePackageVersion

prepareIndexPkgCabal :: PackageId -> UTCTime -> FilePath -> Action Tar.Entry
prepareIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BS.readFile filePath
  pure $ mkTarEntry (BL.fromStrict contents) (IndexPkgCabal pkgId) timestamp

prepareIndexPkgMetadata :: Maybe UTCTime -> PreparedPackageVersion -> Action Targets
prepareIndexPkgMetadata expiryTime PreparedPackageVersion{pkgId, sdistPath} = do
  targetFileInfo <- liftIO $ computeFileInfoSimple' sdistPath
  let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
  return
    Targets
      { targetsVersion = FileVersion 1
      , targetsExpires = FileExpires expiryTime
      , targetsTargets = fromList [(TargetPathRepo packagePath, targetFileInfo)]
      , targetsDelegations = Nothing
      }

-- Currently `extraEntries` are only used for encoding `prefered-versions`.
getExtraEntries :: [PreparedPackageVersion] -> [Tar.Entry]
getExtraEntries packageVersions =
  let
    -- Group all (package) versions by package (name)
    groupedPackageVersions :: [NE.NonEmpty PreparedPackageVersion]
    groupedPackageVersions = NE.groupWith (pkgName . pkgId) packageVersions

    -- All versions of a given package together form a list of entries
    -- The list of entries might be empty (in case no version has been deprecated)
    generateEntriesForGroup :: NE.NonEmpty PreparedPackageVersion -> [Tar.Entry]
    generateEntriesForGroup packageGroup = map createTarEntry effectiveRanges
     where
      -- Get the package name of the current group.
      pn :: PackageName
      pn = pkgName $ pkgId $ NE.head packageGroup
      -- Collect and sort the deprecation changes for the package group, turning them into a action on VersionRange
      deprecationChanges :: [(UTCTime, VersionRange -> VersionRange)]
      deprecationChanges = sortOn fst $ foldMap versionDeprecationChanges packageGroup
      -- Calculate (by applying them chronologically) the effective `VersionRange` for the package group.
      effectiveRanges :: [(UTCTime, VersionRange)]
      effectiveRanges = NE.tail $ NE.scanl applyChangeToRange (posixSecondsToUTCTime 0, anyVersion) deprecationChanges

      -- Create a `Tar.Entry` for the package group, its computed `VersionRange` and a timestamp.
      createTarEntry (ts, effectiveRange) = mkTarEntry (BL.pack $ prettyShow dep) (IndexPkgPrefs pn) ts
       where
        -- Cabal uses `Dependency` to represent preferred versions, cf.
        -- `parsePreferredVersions`. The (sub)libraries part is ignored.
        dep = mkDependency pn effectiveRange mainLibSet
   in
    foldMap generateEntriesForGroup groupedPackageVersions

-- TODO: the functions belows should be moved to Foliage.PreparedPackageVersion

-- Extract deprecation changes for a given `PreparedPackageVersion`.
versionDeprecationChanges :: PreparedPackageVersion -> [(UTCTime, VersionRange -> VersionRange)]
versionDeprecationChanges
  PreparedPackageVersion
    { pkgId = PackageIdentifier{pkgVersion}
    , pkgVersionDeprecationChanges
    } =
    map (second $ applyDeprecation pkgVersion) pkgVersionDeprecationChanges

-- Apply a given change (`VersionRange -> VersionRange`) to a `VersionRange` and
-- return the simplified the result with a new timestamp.
applyChangeToRange :: (UTCTime, VersionRange) -> (UTCTime, VersionRange -> VersionRange) -> (UTCTime, VersionRange)
applyChangeToRange (_, range) (ts, change) = (ts, simplifyVersionRange $ change range)

-- Exclude (or include) to the `VersionRange` of prefered versions, a given
-- `Version`, if the `Version` is (or not) tagged as "deprecated".
applyDeprecation :: Version -> Bool -> VersionRange -> VersionRange
applyDeprecation pkgVersion deprecated =
  if deprecated
    then intersectVersionRanges (notThisVersion pkgVersion)
    else unionVersionRanges (thisVersion pkgVersion)

mkTarEntry :: BL.ByteString -> IndexFile dec -> UTCTime -> Tar.Entry
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

data TarContents = TarContents
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult TarContents = BL.ByteString
