module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, when)
import Data.ByteString.Lazy qualified as BSL
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Foliage.HackageSecurity
import Foliage.Meta
import Foliage.Options
import Foliage.PrepareSdist
import Foliage.PrepareSource (addPrepareSourceRule, prepareSource)
import Foliage.RemoteAsset (addFetchRemoteAssetRule)
import Foliage.Shake
import Foliage.Time qualified as Time
import Hackage.Security.Util.Path (castRoot, toFilePath)

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  outputDirRoot <- liftIO $ makeAbsolute (fromFilePath (buildOptsOutputDir buildOptions))
  shake opts $
    do
      addFetchRemoteAssetRule cacheDir
      addPrepareSourceRule (buildOptsInputDir buildOptions) cacheDir
      addPrepareSdistRule outputDirRoot
      phony "buildAction" (buildAction buildOptions)
      want ["buildAction"]
  where
    cacheDir = "_cache"
    opts =
      shakeOptions
        { shakeFiles = cacheDir,
          shakeVerbosity = Diagnostic
        }

buildAction :: BuildOptions -> Action ()
buildAction
  BuildOptions
    { buildOptsSignOpts = signOpts,
      buildOptsCurrentTime = mCurrentTime,
      buildOptsExpireSignaturesOn = mExpireSignaturesOn,
      buildOptsInputDir = inputDir,
      buildOptsOutputDir = outputDir
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
        return $ const $ return []

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

    packages <- getPackages inputDir

    allCabalFiles <-
      concat
        <$> for
          packages
          ( \(pkgId, pkgMeta) -> do
              let PackageVersionMeta {packageVersionTimestamp, packageVersionRevisions} = pkgMeta
              srcDir <- prepareSource pkgId pkgMeta
              let cabalFilePath = srcDir </> unPackageName (pkgName pkgId) <.> "cabal"
              let cabalFileTimestamp = fromMaybe currentTime packageVersionTimestamp
              return $
                (pkgId, cabalFileTimestamp, cabalFilePath) :
                map
                  ( \RevisionMeta {revisionTimestamp, revisionNumber} ->
                      (pkgId, revisionTimestamp, cabalFileRevisionPath inputDir pkgId revisionNumber)
                  )
                  packageVersionRevisions
          )

    entries1 <- for allCabalFiles $ \(pkgId, timestamp, filePath) ->
      prepareIndexPkgCabal pkgId timestamp filePath

    targetKeys <- maybeReadKeysAt "target"
    entries2 <- for packages $ uncurry (prepareIndexPkgMetadata currentTime expiryTime targetKeys)

    let tarContents = Tar.write $ sortOn Tar.entryTime (entries1 ++ entries2)
    traced "Writing index" $ do
      BSL.writeFile (anchorPath outputDirRoot repoLayoutIndexTar) tarContents
      BSL.writeFile (anchorPath outputDirRoot repoLayoutIndexTarGz) $ GZip.compress tarContents

    privateKeysRoot <- maybeReadKeysAt "root"
    privateKeysTarget <- maybeReadKeysAt "target"
    privateKeysSnapshot <- maybeReadKeysAt "snapshot"
    privateKeysTimestamp <- maybeReadKeysAt "timestamp"
    privateKeysMirrors <- maybeReadKeysAt "mirrors"

    liftIO $
      writeSignedJSON outputDirRoot repoLayoutMirrors privateKeysMirrors $
        Mirrors
          { mirrorsVersion = FileVersion 1,
            mirrorsExpires = FileExpires expiryTime,
            mirrorsMirrors = []
          }

    liftIO $
      writeSignedJSON outputDirRoot repoLayoutRoot privateKeysRoot $
        Root
          { rootVersion = FileVersion 1,
            rootExpires = FileExpires expiryTime,
            rootKeys =
              fromKeys $
                concat
                  [ privateKeysRoot,
                    privateKeysTarget,
                    privateKeysSnapshot,
                    privateKeysTimestamp,
                    privateKeysMirrors
                  ],
            rootRoles =
              RootRoles
                { rootRolesRoot =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysRoot,
                        roleSpecThreshold = KeyThreshold 2
                      },
                  rootRolesSnapshot =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysSnapshot,
                        roleSpecThreshold = KeyThreshold 1
                      },
                  rootRolesTargets =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysTarget,
                        roleSpecThreshold = KeyThreshold 1
                      },
                  rootRolesTimestamp =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysTimestamp,
                        roleSpecThreshold = KeyThreshold 1
                      },
                  rootRolesMirrors =
                    RoleSpec
                      { roleSpecKeys = map somePublicKey privateKeysMirrors,
                        roleSpecThreshold = KeyThreshold 1
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
          { snapshotVersion = FileVersion 1,
            snapshotExpires = FileExpires expiryTime,
            snapshotInfoRoot = rootInfo,
            snapshotInfoMirrors = mirrorsInfo,
            snapshotInfoTar = Just tarInfo,
            snapshotInfoTarGz = tarGzInfo
          }

    snapshotInfo <- computeFileInfoSimple' (anchorPath outputDirRoot repoLayoutSnapshot)
    liftIO $
      writeSignedJSON outputDirRoot repoLayoutTimestamp privateKeysTimestamp $
        Timestamp
          { timestampVersion = FileVersion 1,
            timestampExpires = FileExpires expiryTime,
            timestampInfoSnapshot = snapshotInfo
          }

getPackages :: FilePath -> Action [(PackageIdentifier, PackageVersionMeta)]
getPackages inputDir = do
  metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

  when (null metaFiles) $ do
    putError $
      unlines
        [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)",
          "Make sure you are passing the right input directory. The default input directory is _sources"
        ]
    fail "no package metadata found"

  for metaFiles $ \metaFile -> do
    let [pkgName, pkgVersion, _] = splitDirectories metaFile
    let Just name = simpleParsec pkgName
    let Just version = simpleParsec pkgVersion
    let pkgId = PackageIdentifier name version

    meta <-
      readPackageVersionMeta' (inputDir </> metaFile) >>= \case
        PackageVersionMeta {packageVersionRevisions, packageVersionTimestamp = Nothing}
          | not (null packageVersionRevisions) -> do
            putError $
              inputDir </> metaFile <> " has cabal file revisions but the original package has no timestamp. This combination doesn't make sense. Either add a timestamp on the original package or remove the revisions"
            fail "invalid package metadata"
        PackageVersionMeta {packageVersionRevisions, packageVersionTimestamp = Just pkgTs}
          | any ((< pkgTs) . revisionTimestamp) packageVersionRevisions -> do
            putError $
              inputDir </> metaFile <> " has a revision with timestamp earlier than the package itself. Adjust the timestamps so that all revisions come after the original package"
            fail "invalid package metadata"
        meta ->
          return meta
    return (pkgId, meta)

prepareIndexPkgCabal :: PackageId -> UTCTime -> FilePath -> Action Tar.Entry
prepareIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BSL.readFile filePath
  return $ mkTarEntry contents (IndexPkgCabal pkgId) timestamp

prepareIndexPkgMetadata :: UTCTime -> Maybe UTCTime -> [Some Key] -> PackageIdentifier -> PackageVersionMeta -> Action Tar.Entry
prepareIndexPkgMetadata currentTime expiryTime keys pkgId pkgMeta = do
  let PackageVersionMeta {packageVersionTimestamp} = pkgMeta
  sdist <- prepareSdist pkgId pkgMeta
  targetFileInfo <- computeFileInfoSimple' sdist
  let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
  let targets =
        Targets
          { targetsVersion = FileVersion 1,
            targetsExpires = FileExpires expiryTime,
            targetsTargets = fromList [(TargetPathRepo packagePath, targetFileInfo)],
            targetsDelegations = Nothing
          }

  return $
    mkTarEntry
      (renderSignedJSON keys targets)
      (IndexPkgMetadata pkgId)
      (fromMaybe currentTime packageVersionTimestamp)

mkTarEntry :: BSL.ByteString -> IndexFile dec -> UTCTime -> Tar.Entry
mkTarEntry contents indexFile timestamp =
  (Tar.fileEntry tarPath contents)
    { Tar.entryTime = floor $ Time.utcTimeToPOSIXSeconds timestamp,
      Tar.entryOwnership =
        Tar.Ownership
          { Tar.ownerName = "foliage",
            Tar.groupName = "foliage",
            Tar.ownerId = 0,
            Tar.groupId = 0
          }
    }
  where
    indexPath = toFilePath $ castRoot $ indexFileToPath hackageIndexLayout indexFile
    Right tarPath = Tar.toTarPath False indexPath

anchorPath :: Path Absolute -> (RepoLayout -> RepoPath) -> FilePath
anchorPath outputDirRoot p =
  toFilePath $ anchorRepoPathLocally outputDirRoot $ p hackageRepoLayout

cabalFileRevisionPath :: FilePath -> PackageIdentifier -> Int -> FilePath
cabalFileRevisionPath inputDir PackageIdentifier {pkgName, pkgVersion} revisionNumber =
  inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "revisions" </> show revisionNumber <.> "cabal"
