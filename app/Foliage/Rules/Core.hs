{-# LANGUAGE TypeFamilies #-}

module Foliage.Rules.Core (
  coreRules,
) where

import Data.Function ((&))
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as C8L
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Key.Env (fromKeys)
import Hackage.Security.Server (
  FileExpires,
  FileVersion (..),
  IndexFile (..),
  IndexLayout (..),
  KeyThreshold (..),
  Mirrors (..),
  RepoLayout (..),
  RoleSpec (..),
  Root (..),
  RootRoles (..),
  Snapshot (..),
  TargetPath (..),
  Targets (..),
  Timestamp (..),
  hackageIndexLayout,
  hackageRepoLayout,
  somePublicKey,
 )
import Hackage.Security.TUF.FileMap qualified as FM
import Hackage.Security.Util.Path qualified as Sec

import Foliage.HackageSecurity
import Foliage.Meta
import Foliage.Rules.Utils

coreRules
  :: FilePath
  -> (PackageIdentifier -> FilePath)
  -> (PackageId -> FilePath)
  -> UTCTime
  -> Rules ()
coreRules outputDir cabalFileForPkgId sdistPathForPkgId currentTime = do
  action $ do
    -- This is all that is necessary since the single source distributions
    -- are required to build 01-index.tar.
    need
      [ outputDir </> "mirrors.json"
      , outputDir </> "root.json"
      , outputDir </> "snapshot.json"
      , outputDir </> "timestamp.json"
      , outputDir </> "01-index.tar"
      , outputDir </> "01-index.tar.gz"
      ]

  getIndexEntries <- do
    getIndexEntries <- newCache $ \IndexEntries{} -> do
      pkgSpecs <- askOracle PkgSpecs
      cabalEntries <- makeCabalEntries currentTime cabalFileForPkgId pkgSpecs
      metadataEntries <- makeMetadataEntries currentTime sdistPathForPkgId pkgSpecs
      let extraEntries = makeExtraEntries pkgSpecs

      -- WARN: See note on `makeCabalEntries`, the sorting here has to be stable
      return $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries ++ extraEntries)
    -- Return the oracle function applied to IndexEntries ()
    return $ getIndexEntries $ IndexEntries ()

  outputDir </> "mirrors.json" %> mkMirrors

  outputDir </> "root.json" %> mkRoot

  outputDir </> "snapshot.json" %> mkSnapshot outputDir

  outputDir </> "timestamp.json" %> mkTimestamp outputDir

  outputDir </> "01-index.tar" %> \path ->
    getIndexEntries >>= liftIO . BSL.writeFile path . Tar.write

  outputDir </> "01-index.tar.gz" %> \path ->
    getIndexEntries >>= liftIO . BSL.writeFile path . GZip.compress . Tar.write

  outputDir </> "package/*.tar.gz" %> \path ->
    case filePattern (outputDir </> "package/*.tar.gz") path of
      Just [pkgIdStr] | Just pkgId <- simpleParsec pkgIdStr -> do
        let cabalFilePath = cabalFileForPkgId pkgId
        need [cabalFilePath]
        pkgDesc <- liftIO $ readGenericPackageDescription Verbosity.silent cabalFilePath
        liftIO $ packageDirToSdist Verbosity.normal pkgDesc (takeDirectory cabalFilePath) >>= BSL.writeFile path
      _ -> error $ "The path " ++ path ++ " does not correspond to a valid package"

makeCabalEntries :: UTCTime -> (PackageId -> FilePath) -> Map PackageId (FilePath, PackageVersionSpec) -> Action [Tar.Entry]
makeCabalEntries currentTime cabalFileForPkgId allPkgSpecs = do
  flip M.foldMapWithKey allPkgSpecs $ \pkgId (metaFile, pkgSpec) -> do
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

makeMetadataEntries :: UTCTime -> (PackageId -> FilePath) -> Map PackageId (FilePath, PackageVersionSpec) -> Action [Tar.Entry]
makeMetadataEntries currentTime sdistPathForPkgId allPkgSpecs = do
  Just expiryTime <- getShakeExtra

  targetKeys <- readKeys "target"

  flip M.foldMapWithKey allPkgSpecs $ \pkgId (_metaFile, pkgSpec) -> do
    let sdistPath = sdistPathForPkgId pkgId
        pkgTimestamp = fromMaybe currentTime (packageVersionTimestamp pkgSpec)
    targets <- makeIndexPkgMetadata expiryTime pkgId sdistPath
    return [mkTarEntry (renderSignedJSON targetKeys targets) (IndexPkgMetadata pkgId) pkgTimestamp]

-- Currently `extraEntries` are only used for encoding `prefered-versions`.
makeExtraEntries :: Map PackageId (FilePath, PackageVersionSpec) -> [Tar.Entry]
makeExtraEntries = M.foldMapWithKey generateEntriesForGroup . groupByPackageName

-- All versions of a given package together form a list of entries
-- The list of entries might be empty (in case no version has been deprecated)
generateEntriesForGroup :: PackageName -> NE.NonEmpty (Version, PackageVersionSpec) -> [Tar.Entry]
generateEntriesForGroup pkgName packageGroup =
  map createTarEntry effectiveRanges
 where
  -- Collect and sort the deprecation changes for the package group, turning them into a action on VersionRange
  deprecationChanges :: [(UTCTime, VersionRange -> VersionRange)]
  deprecationChanges =
    packageGroup
      & foldMap
        ( \(pkgVersion, pkgSpec) ->
            [ (deprecationTimestamp, rangeAct)
            | DeprecationSpec{deprecationTimestamp, deprecationIsDeprecated} <- packageVersionDeprecations pkgSpec
            , let rangeAct =
                    if deprecationIsDeprecated
                      then intersectVersionRanges (notThisVersion pkgVersion)
                      else unionVersionRanges (thisVersion pkgVersion)
            ]
        )

  -- Calculate (by applying them chronologically) the effective `VersionRange` for the package group.
  effectiveRanges :: [(UTCTime, VersionRange)]
  effectiveRanges =
    NE.tail $
      NE.scanl
        applyChangeToRange
        (posixSecondsToUTCTime 0, anyVersion)
        (sortOn fst deprecationChanges)

  -- Apply a given change (`VersionRange -> VersionRange`) to a `VersionRange` and
  -- return the simplified the result with a new timestamp.
  applyChangeToRange
    :: (UTCTime, VersionRange)
    -> (UTCTime, VersionRange -> VersionRange)
    -> (UTCTime, VersionRange)
  applyChangeToRange (_, range) (ts, change) = (ts, simplifyVersionRange $ change range)

  -- Create a `Tar.Entry` for the package group, its computed `VersionRange` and a timestamp.
  createTarEntry (ts, effectiveRange) = mkTarEntry (C8L.pack $ prettyShow dep) (IndexPkgPrefs pkgName) ts
   where
    -- Cabal uses `Dependency` to represent preferred versions, cf.
    -- `parsePreferredVersions`. The (sub)libraries part is ignored.
    dep = mkDependency pkgName effectiveRange mainLibSet

makeIndexPkgCabal :: PackageId -> UTCTime -> FilePath -> Action Tar.Entry
makeIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BS.readFile filePath
  pure $ mkTarEntry (C8L.fromStrict contents) (IndexPkgCabal pkgId) timestamp

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

mkTarEntry
  :: BSL.ByteString
  -> IndexFile dec
  -> UTCTime
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

mkMirrors :: FilePath -> Action ()
mkMirrors path = do
  Just expiryTime <- getShakeExtra
  privateKeysMirrors <- readKeys "mirrors"

  trackWrite [path]
  liftIO $
    writeSignedJSON path privateKeysMirrors $
      Mirrors
        { mirrorsVersion = FileVersion 1
        , mirrorsExpires = expiryTime
        , mirrorsMirrors = []
        }

mkRoot :: FilePath -> Action ()
mkRoot path = do
  Just expiryTime <- getShakeExtra

  privateKeysRoot <- readKeys "root"
  privateKeysTarget <- readKeys "target"
  privateKeysSnapshot <- readKeys "snapshot"
  privateKeysTimestamp <- readKeys "timestamp"
  privateKeysMirrors <- readKeys "mirrors"

  trackWrite [path]
  liftIO $
    writeSignedJSON path privateKeysRoot $
      Root
        { rootVersion = FileVersion 1
        , rootExpires = expiryTime
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

mkSnapshot :: FilePath -> FilePath -> Action ()
mkSnapshot outputDir path = do
  Just expiryTime <- getShakeExtra
  privateKeysSnapshot <- readKeys "snapshot"

  rootInfo <- computeFileInfoSimple $ outputDir </> "root.json"
  mirrorsInfo <- computeFileInfoSimple $ outputDir </> "mirrors.json"
  tarInfo <- computeFileInfoSimple $ outputDir </> "01-index.tar"
  tarGzInfo <- computeFileInfoSimple $ outputDir </> "01-index.tar.gz"

  trackWrite [path]
  liftIO $
    writeSignedJSON path privateKeysSnapshot $
      Snapshot
        { snapshotVersion = FileVersion 1
        , snapshotExpires = expiryTime
        , snapshotInfoRoot = rootInfo
        , snapshotInfoMirrors = mirrorsInfo
        , snapshotInfoTar = Just tarInfo
        , snapshotInfoTarGz = tarGzInfo
        }

mkTimestamp :: FilePath -> FilePath -> Action ()
mkTimestamp outputDir path = do
  Just expiryTime <- getShakeExtra
  privateKeysTimestamp <- readKeys "timestamp"

  snapshotInfo <- computeFileInfoSimple $ outputDir </> "snapshot.json"

  trackWrite [path]
  liftIO $
    writeSignedJSON path privateKeysTimestamp $
      Timestamp
        { timestampVersion = FileVersion 1
        , timestampExpires = expiryTime
        , timestampInfoSnapshot = snapshotInfo
        }
