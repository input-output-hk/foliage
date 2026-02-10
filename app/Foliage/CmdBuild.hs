{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Foliage.GitClone (addGitCloneRule)
import Foliage.HackageSecurity hiding (ToJSON, toJSON)
import Foliage.Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Pages
import Foliage.PreparePackageVersion (PreparedPackageVersion (..), preparePackageVersion)
import Foliage.PrepareSdist (addPrepareSdistRule)
import Foliage.PrepareSource (addPrepareSourceRule)
import Foliage.Shake
import Foliage.Time qualified as Time
import Hackage.Security.Util.Path (castRoot, toFilePath)
import System.Directory (createDirectoryIfMissing)

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  outputDirRoot <- makeAbsolute (fromFilePath (buildOptsOutputDir buildOptions))
  shake opts $
    do
      addFetchURLRule cacheDir
      addGitCloneRule cacheDir
      addPrepareSourceRule (buildOptsInputDir buildOptions) cacheDir
      addPrepareSdistRule outputDirRoot
      phony "buildAction" (buildAction buildOptions)
      want ["buildAction"]
 where
  cacheDir = "_cache"
  opts =
    shakeOptions
      { shakeFiles = cacheDir
      , shakeVerbosity = buildOptsVerbosity buildOptions
      , shakeThreads = buildOptsNumThreads buildOptions
      }

buildAction :: BuildOptions -> Action ()
buildAction
  BuildOptions
    { buildOptsSignOpts = signOpts
    , buildOptsCurrentTime = mCurrentTime
    , buildOptsExpireSignaturesOn = mExpireSignaturesOn
    , buildOptsInputDir = inputDir
    , buildOptsOutputDir = outputDir
    , buildOptsWriteMetadata = doWritePackageMeta
    } = do
    outputDirRoot <- liftIO $ makeAbsolute (fromFilePath outputDir)

    maybeReadKeysAt <- case signOpts of
      SignOptsSignWithKeys keysPath -> do
        ks <- doesDirectoryExist keysPath
        unless ks $ do
          putWarn $ "You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
          liftIO $ createKeys keysPath
        return $ \name -> readKeysAt (keysPath </> name)
      SignOptsDon'tSign ->
        return $ const $ pure []

    expiryTime <-
      for mExpireSignaturesOn $ \expireSignaturesOn -> do
        putInfo $ "Expiry time set to " <> Time.iso8601Show expireSignaturesOn
        return expireSignaturesOn

    currentTime <- case mCurrentTime of
      Nothing -> do
        t <- Time.truncateSeconds <$> liftIO Time.getCurrentTime
        putInfo $ "Current time set to " <> Time.iso8601Show t <> ". You can set a fixed time using the --current-time option."
        return t
      Just t -> do
        putInfo $ "Current time set to " <> Time.iso8601Show t <> "."
        return t

    packageVersions <- getPackageVersions inputDir

    makeIndexPage outputDir

    makeAllPackagesPage currentTime outputDir packageVersions

    makeAllPackageVersionsPage currentTime outputDir packageVersions

    void $ forP packageVersions $ makePackageVersionPage outputDir

    when doWritePackageMeta $
      makeMetadataFile outputDir packageVersions

    cabalEntries <-
      foldMap
        ( \PreparedPackageVersion{pkgId, pkgTimestamp, cabalFilePath, originalCabalFilePath, cabalFileRevisions} -> do
            -- original cabal file, with its timestamp (if specified)
            copyFileChanged originalCabalFilePath (outputDir </> "package" </> prettyShow pkgId </> "revision" </> "0" <.> "cabal")
            cf <- prepareIndexPkgCabal pkgId (fromMaybe currentTime pkgTimestamp) originalCabalFilePath

            -- all revised cabal files, with their timestamp
            revcf <- for cabalFileRevisions $ \(RevisionSpec{revisionTimestamp, revisionNumber}, path) -> do
              copyFileChanged path (outputDir </> "package" </> prettyShow pkgId </> "revision" </> show revisionNumber <.> "cabal")
              prepareIndexPkgCabal pkgId revisionTimestamp path

            -- current version of the cabal file (after the revisions, if any)
            copyFileChanged cabalFilePath (outputDir </> "package" </> prettyShow pkgId </> prettyShow (pkgName pkgId) <.> "cabal")

            -- WARN: So far Foliage allows publishing a package and a cabal file revision with the same timestamp
            -- This accidentally works because 1) the following inserts the original cabal file before the revisions
            -- AND 2) Data.List.sortOn is stable. The revised cabal file will always be after the original one.
            return $ cf : revcf
        )
        packageVersions

    targetKeys <- maybeReadKeysAt "target"

    metadataEntries <-
      forP packageVersions $ \ppv@PreparedPackageVersion{pkgId, pkgTimestamp} -> do
        targets <- prepareIndexPkgMetadata expiryTime ppv
        pure $
          mkTarEntry
            (renderSignedJSON targetKeys targets)
            (IndexPkgMetadata pkgId)
            (fromMaybe currentTime pkgTimestamp)

    let extraEntries = getExtraEntries packageVersions

    -- WARN: See note above, the sorting here has to be stable
    let tarContents = Tar.write $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries ++ extraEntries)
    traced "Writing index" $ do
      BL.writeFile (anchorPath outputDirRoot repoLayoutIndexTar) tarContents
      BL.writeFile (anchorPath outputDirRoot repoLayoutIndexTarGz) $ GZip.compress tarContents

    privateKeysRoot <- maybeReadKeysAt "root"
    privateKeysTarget <- maybeReadKeysAt "target"
    privateKeysSnapshot <- maybeReadKeysAt "snapshot"
    privateKeysTimestamp <- maybeReadKeysAt "timestamp"
    privateKeysMirrors <- maybeReadKeysAt "mirrors"

    liftIO $
      writeSignedJSON outputDirRoot repoLayoutMirrors privateKeysMirrors $
        Mirrors
          { mirrorsVersion = FileVersion 1
          , mirrorsExpires = FileExpires expiryTime
          , mirrorsMirrors = []
          }

    liftIO $
      writeSignedJSON outputDirRoot repoLayoutRoot privateKeysRoot $
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

    rootInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutRoot)
    mirrorsInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutMirrors)
    tarInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutIndexTar)
    tarGzInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutIndexTarGz)

    liftIO $
      writeSignedJSON outputDirRoot repoLayoutSnapshot privateKeysSnapshot $
        Snapshot
          { snapshotVersion = FileVersion 1
          , snapshotExpires = FileExpires expiryTime
          , snapshotInfoRoot = rootInfo
          , snapshotInfoMirrors = mirrorsInfo
          , snapshotInfoTar = Just tarInfo
          , snapshotInfoTarGz = tarGzInfo
          }

    snapshotInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutSnapshot)
    liftIO $
      writeSignedJSON outputDirRoot repoLayoutTimestamp privateKeysTimestamp $
        Timestamp
          { timestampVersion = FileVersion 1
          , timestampExpires = FileExpires expiryTime
          , timestampInfoSnapshot = snapshotInfo
          }

makeMetadataFile :: FilePath -> [PreparedPackageVersion] -> Action ()
makeMetadataFile outputDir packageVersions = traced "writing metadata" $ do
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

getPackageVersions :: FilePath -> Action [PreparedPackageVersion]
getPackageVersions inputDir = do
  metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

  when (null metaFiles) $ do
    error $
      unlines
        [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)"
        , "Make sure you are passing the right input directory. The default input directory is _sources"
        ]

  forP metaFiles $ preparePackageVersion inputDir

prepareIndexPkgCabal :: PackageId -> UTCTime -> FilePath -> Action Tar.Entry
prepareIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BS.readFile filePath
  pure $ mkTarEntry (BL.fromStrict contents) (IndexPkgCabal pkgId) timestamp

prepareIndexPkgMetadata :: Maybe UTCTime -> PreparedPackageVersion -> Action Targets
prepareIndexPkgMetadata expiryTime PreparedPackageVersion{pkgId, sdistPath} = do
  targetFileInfo <- liftIO $ computeFileInfoSimple sdistPath
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

  indexPath = toFilePath $ castRoot $ indexFileToPath hackageIndexLayout indexFile

anchorPath :: Path Absolute -> (RepoLayout -> RepoPath) -> FilePath
anchorPath outputDirRoot p =
  toFilePath $ anchorRepoPathLocally outputDirRoot $ p hackageRepoLayout
