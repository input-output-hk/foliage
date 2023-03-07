{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, void, when)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Foliage.HackageSecurity hiding (ToJSON, toJSON)
import Foliage.Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Pages
import Foliage.PreparePackageVersion (PreparedPackageVersion (..), preparePackageVersion)
import Foliage.PrepareSdist (addPrepareSdistRule)
import Foliage.PrepareSource (addPrepareSourceRule)
import Foliage.RemoteAsset (addFetchRemoteAssetRule)
import Foliage.Shake
import Foliage.Time qualified as Time
import Hackage.Security.Util.Path (castRoot, toFilePath)
import Network.URI (URI (uriPath, uriQuery, uriScheme), nullURI)
import System.Directory (createDirectoryIfMissing)

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
          shakeVerbosity = Verbose,
          shakeThreads = buildOptsNumThreads buildOptions
        }

buildAction :: BuildOptions -> Action ()
buildAction
  BuildOptions
    { buildOptsSignOpts = signOpts,
      buildOptsCurrentTime = mCurrentTime,
      buildOptsExpireSignaturesOn = mExpireSignaturesOn,
      buildOptsInputDir = inputDir,
      buildOptsOutputDir = outputDir,
      buildOptsWriteMetadata = doWritePackageMeta
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

    packageVersions <- getPackageVersions inputDir

    makeIndexPage outputDir

    makeAllPackagesPage currentTime outputDir packageVersions

    makeAllPackageVersionsPage currentTime outputDir packageVersions

    when doWritePackageMeta $
      makeMetadataFile outputDir packageVersions

    void $ forP packageVersions $ makePackageVersionPage outputDir

    void $ forP packageVersions $ \PreparedPackageVersion {pkgId, cabalFilePath} -> do
      let PackageIdentifier {pkgName, pkgVersion} = pkgId
      copyFileChanged cabalFilePath (outputDir </> "index" </> prettyShow pkgName </> prettyShow pkgVersion </> prettyShow pkgName <.> "cabal")

    cabalEntries <-
      foldMap
        ( \PreparedPackageVersion {pkgId, pkgTimestamp, originalCabalFilePath, cabalFileRevisions} -> do
            -- original cabal file, with its timestamp (if specified)
            let cabalFileTimestamp = fromMaybe currentTime pkgTimestamp
            cf <- prepareIndexPkgCabal pkgId cabalFileTimestamp originalCabalFilePath

            -- all revised cabal files, with their timestamp
            revcf <- for cabalFileRevisions $ uncurry (prepareIndexPkgCabal pkgId)

            return $ cf : revcf
        )
        packageVersions

    targetKeys <- maybeReadKeysAt "target"
    metadataEntries <-
      forP packageVersions $ \ppv@PreparedPackageVersion {pkgId, pkgTimestamp} -> do
        let PackageIdentifier {pkgName, pkgVersion} = pkgId
        targets <- prepareIndexPkgMetadata expiryTime ppv
        let path = outputDir </> "index" </> prettyShow pkgName </> prettyShow pkgVersion </> "package.json"
        liftIO $ BL.writeFile path $ renderSignedJSON targetKeys targets
        pure $
          mkTarEntry
            (renderSignedJSON targetKeys targets)
            (IndexPkgMetadata pkgId)
            (fromMaybe currentTime pkgTimestamp)

    let tarContents = Tar.write $ sortOn Tar.entryTime (cabalEntries ++ metadataEntries)
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

makeMetadataFile :: FilePath -> [PreparedPackageVersion] -> Action ()
makeMetadataFile outputDir packageVersions = traced "writing metadata" $ do
  createDirectoryIfMissing True (outputDir </> "foliage")
  Aeson.encodeFile
    (outputDir </> "foliage" </> "packages.json")
    (map encodePackageVersion packageVersions)
  where
    encodePackageVersion
      PreparedPackageVersion
        { pkgId = PackageIdentifier {pkgName, pkgVersion},
          pkgTimestamp,
          pkgVersionForce,
          pkgVersionSource
        } =
        Aeson.object
          ( [ "pkg-name" Aeson..= pkgName,
              "pkg-version" Aeson..= pkgVersion,
              "url" Aeson..= sourceUrl pkgVersionSource
            ]
              ++ ["forced-version" Aeson..= True | pkgVersionForce]
              ++ (case pkgTimestamp of Nothing -> []; Just t -> ["timestamp" Aeson..= t])
          )

    sourceUrl :: PackageVersionSource -> URI
    sourceUrl (TarballSource uri Nothing) = uri
    sourceUrl (TarballSource uri (Just subdir)) = uri {uriQuery = "?dir=" ++ subdir}
    sourceUrl (GitHubSource repo rev Nothing) =
      nullURI
        { uriScheme = "github:",
          uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev)
        }
    sourceUrl (GitHubSource repo rev (Just subdir)) =
      nullURI
        { uriScheme = "github:",
          uriPath = T.unpack (unGitHubRepo repo) </> T.unpack (unGitHubRev rev),
          uriQuery = "?dir=" ++ subdir
        }

getPackageVersions :: FilePath -> Action [PreparedPackageVersion]
getPackageVersions inputDir = do
  metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

  when (null metaFiles) $ do
    error $
      unlines
        [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)",
          "Make sure you are passing the right input directory. The default input directory is _sources"
        ]

  forP metaFiles $ preparePackageVersion inputDir

prepareIndexPkgCabal :: PackageId -> UTCTime -> FilePath -> Action Tar.Entry
prepareIndexPkgCabal pkgId timestamp filePath = do
  need [filePath]
  contents <- liftIO $ BS.readFile filePath
  pure $ mkTarEntry (BL.fromStrict contents) (IndexPkgCabal pkgId) timestamp

prepareIndexPkgMetadata :: Maybe UTCTime -> PreparedPackageVersion -> Action Targets
prepareIndexPkgMetadata expiryTime PreparedPackageVersion {pkgId, sdistPath} = do
  targetFileInfo <- computeFileInfoSimple' sdistPath
  let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
  return
    Targets
      { targetsVersion = FileVersion 1,
        targetsExpires = FileExpires expiryTime,
        targetsTargets = fromList [(TargetPathRepo packagePath, targetFileInfo)],
        targetsDelegations = Nothing
      }

mkTarEntry :: BL.ByteString -> IndexFile dec -> UTCTime -> Tar.Entry
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
    tarPath = case Tar.toTarPath False indexPath of
      Left e -> error $ "Invalid tar path " ++ indexPath ++ "(" ++ e ++ ")"
      Right tp -> tp

    indexPath = toFilePath $ castRoot $ indexFileToPath hackageIndexLayout indexFile

anchorPath :: Path Absolute -> (RepoLayout -> RepoPath) -> FilePath
anchorPath outputDirRoot p =
  toFilePath $ anchorRepoPathLocally outputDirRoot $ p hackageRepoLayout
