{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, when)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.List (dropWhileEnd, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Development.Shake
import Development.Shake.FilePath
import Foliage.HackageSecurity
import Foliage.Meta
import Foliage.Options
import Foliage.Package
import Foliage.Shake
import Foliage.Shake.Oracle
import Foliage.Time qualified as Time
import System.Directory qualified as IO
import System.FilePath.Posix qualified as Posix

cmdBuild :: BuildOptions -> IO ()
cmdBuild
  BuildOptions
    { buildOptsKeysPath = keysPath,
      buildOptsCurrentTime = mCurrentTime,
      buildOptsInputDir = inputDir,
      buildOptsOutputDir = outputDir
    } = do
    ks <- IO.doesDirectoryExist keysPath
    unless ks $ do
      putStrLn $ "🗝️ You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
      createKeys keysPath

    let opts =
          shakeOptions
            { shakeChange = ChangeDigest,
              shakeFiles = "_cache"
            }

    shake opts $ do
      --
      -- Oracles
      --
      getCurrentTime <- addOracle $ \GetCurrentTime ->
        case mCurrentTime of
          Nothing -> do
            t <- Time.truncateSeconds <$> liftIO Time.getCurrentTime
            putInfo $
              unlines
                [ "🕐 Current time set to " <> Time.iso8601Show t <> ".",
                  "You can set a fixed time using the --current-time option"
                ]
            return t
          Just t -> do
            putInfo $ "🕐 Current time set to " <> Time.iso8601Show t <> "."
            return t

      getExpiryTime <- addOracle $ \GetExpiryTime -> do
        t <- Time.addUTCTime (Time.nominalDay * 365) <$> getCurrentTime GetCurrentTime
        putInfo $ "🕐 Expiry time set to " <> Time.iso8601Show t <> " (a year from now)."
        return t

      getPackageMeta <- addOracle $ \(GetPackageMeta PackageId {pkgName, pkgVersion}) ->
        readPackageMeta' $ inputDir </> pkgName </> pkgVersion </> "meta.toml"

      preparePackageSource <- addOracle $ \(PreparePackageSource pkgId@PackageId {pkgName, pkgVersion}) -> do
        PackageMeta {packageSource, packageForceVersion} <- getPackageMeta (GetPackageMeta pkgId)

        let srcDir = "_cache" </> "packages" </> pkgName </> pkgVersion

        -- FIXME too much rework?
        -- this action only depends on the tarball and the package metadata

        -- delete everything inside the package source tree
        liftIO $ do
          -- FIXME this should only delete inside srcDir but apparently
          -- also deletes srcDir itself
          removeFiles srcDir ["//*"]
          IO.createDirectoryIfMissing True srcDir

        case packageSource of
          TarballSource url mSubdir -> do
            tarballPath <- fetchUrl url

            withTempDir $ \tmpDir -> do
              cmd_ ["tar", "xzf", tarballPath, "-C", tmpDir]

              -- Special treatment of top-level directory: we remove it
              --
              -- Note: Don't let shake look into tmpDir! it will cause
              -- unnecessary rework because tmpDir is always new
              ls <- liftIO $ IO.getDirectoryContents tmpDir
              let ls' = filter (not . all (== '.')) ls

              let fix1 = case ls' of [l] -> (</> l); _ -> id
                  fix2 = case mSubdir of Just s -> (</> s); _ -> id
                  tdir = fix2 $ fix1 tmpDir

              cmd_ ["cp", "--recursive", "--no-target-directory", "--dereference", tdir, srcDir]

        -- Delete cabal.project files if present
        projectFiles <- liftIO $ filter ("cabal.project" `isPrefixOf`) <$> IO.getDirectoryContents srcDir
        unless (null projectFiles) $ do
          putWarn $ "⚠️ Deleting cabal project files from " ++ srcDir
          liftIO $ for_ projectFiles $ IO.removeFile . (srcDir </>)

        applyPatches inputDir srcDir pkgId

        when packageForceVersion $
          forcePackageVersion srcDir pkgId

        return srcDir

      getPackages <- addOracle $ \GetPackages -> do
        metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

        when (null metaFiles) $ do
          putError $
            unlines
              [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)",
                "Make sure you are passing the right input directory. The default input directory is _sources"
              ]
          fail "no package metadata found"

        return $
          [ PackageId pkgName pkgVersion
            | path <- metaFiles,
              let [pkgName, pkgVersion, _] = splitDirectories path
          ]

      --
      -- Entrypoint
      --

      -- This triggers the whole chain of TUF metadata
      want [outputDir </> "timestamp.json"]

      -- This build the current index entry for all packages
      action $ do
        pkgIds <- getPackages GetPackages
        need
          [ outputDir </> "index" </> pkgName </> pkgVersion </> pkgName <.> "cabal"
            | PackageId pkgName pkgVersion <- pkgIds
          ]

      --
      -- timestamp.json
      --
      outputDir </> "timestamp.json" %> \path -> do
        snapshotInfo <- computeFileInfoSimple' (outputDir </> "snapshot.json")
        expires <- getExpiryTime GetExpiryTime
        let timestamp =
              Timestamp
                { timestampVersion = FileVersion 1,
                  timestampExpires = FileExpires (Just expires),
                  timestampInfoSnapshot = snapshotInfo
                }

        keys <- readKeysAt (keysPath </> "timestamp")
        let timestampSigned = withSignatures hackageRepoLayout keys timestamp
        liftIO $ do
          p <- makeAbsolute (fromFilePath path)
          writeJSON hackageRepoLayout p timestampSigned
        putInfo $ "✅ Written " <> path

      --
      -- snapshot.json
      --

      outputDir </> "snapshot.json" %> \path -> do
        rootInfo <- computeFileInfoSimple' (outputDir </> "root.json")
        mirrorsInfo <- computeFileInfoSimple' (outputDir </> "mirrors.json")
        tarInfo <- computeFileInfoSimple' (outputDir </> "01-index.tar")
        tarGzInfo <- computeFileInfoSimple' (outputDir </> "01-index.tar.gz")
        expires <- getExpiryTime GetExpiryTime
        let snapshot =
              Snapshot
                { snapshotVersion = FileVersion 1,
                  snapshotExpires = FileExpires (Just expires),
                  snapshotInfoRoot = rootInfo,
                  snapshotInfoMirrors = mirrorsInfo,
                  snapshotInfoTar = Just tarInfo,
                  snapshotInfoTarGz = tarGzInfo
                }

        keys <- readKeysAt (keysPath </> "snapshot")
        let snapshotSigned = withSignatures hackageRepoLayout keys snapshot
        liftIO $ do
          p <- makeAbsolute (fromFilePath path)
          writeJSON hackageRepoLayout p snapshotSigned
        putInfo $ "✅ Written " <> path

      --
      -- root.json
      --

      outputDir </> "root.json" %> \path -> do
        expires <- getExpiryTime GetExpiryTime

        privateKeysRoot <- readKeysAt (keysPath </> "root")
        privateKeysTarget <- readKeysAt (keysPath </> "target")
        privateKeysSnapshot <- readKeysAt (keysPath </> "snapshot")
        privateKeysTimestamp <- readKeysAt (keysPath </> "timestamp")
        privateKeysMirrors <- readKeysAt (keysPath </> "mirrors")

        let root =
              Root
                { rootVersion = FileVersion 1,
                  rootExpires = FileExpires (Just expires),
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

        keys <- readKeysAt (keysPath </> "root")
        let signedRoot = withSignatures hackageRepoLayout keys root
        liftIO $ do
          p <- makeAbsolute (fromFilePath path)
          writeJSON hackageRepoLayout p signedRoot
        putInfo $ "✅ Written " <> path

      --
      -- mirrors.json
      --

      outputDir </> "mirrors.json" %> \path -> do
        expires <- getExpiryTime GetExpiryTime
        let mirrors =
              Mirrors
                { mirrorsVersion = FileVersion 1,
                  mirrorsExpires = FileExpires (Just expires),
                  mirrorsMirrors = []
                }

        keys <- readKeysAt (keysPath </> "mirrors")
        let signedMirrors = withSignatures hackageRepoLayout keys mirrors
        liftIO $ do
          p <- makeAbsolute (fromFilePath path)
          writeJSON hackageRepoLayout p signedMirrors
        putInfo $ "✅ Written " <> path

      --
      -- 01-index.tar
      --

      outputDir </> "01-index.tar" %> \path -> do
        pkgIds <- getPackages GetPackages

        entries <-
          fmap concat $
            for pkgIds $ \pkgId -> do
              let PackageId {pkgName, pkgVersion} = pkgId
              PackageMeta {packageTimestamp, packageRevisions} <- getPackageMeta (GetPackageMeta pkgId)

              srcDir <- preparePackageSource $ PreparePackageSource pkgId
              now <- getCurrentTime GetCurrentTime

              sequence $
                [ -- original cabal file
                  mkTarEntry
                    (srcDir </> pkgName <.> "cabal")
                    (pkgName </> pkgVersion </> pkgName <.> "cabal")
                    (fromMaybe now packageTimestamp),
                  -- package.json
                  mkTarEntry
                    (outputDir </> "index" </> pkgName </> pkgVersion </> "package.json")
                    (pkgName </> pkgVersion </> "package.json")
                    (fromMaybe now packageTimestamp)
                ]
                  ++ [ -- revised cabal files
                       mkTarEntry
                         (inputDir </> pkgName </> pkgVersion </> "revisions" </> show revNum <.> "cabal")
                         (pkgName </> pkgVersion </> pkgName <.> "cabal")
                         (fromMaybe now revTimestamp)
                       | RevisionMeta revTimestamp revNum <- packageRevisions
                     ]

        liftIO $ BSL.writeFile path $ Tar.write (sortOn Tar.entryTime entries)
        putInfo $ "✅ Written " <> path

      --
      -- 01-index.tar.gz
      --

      outputDir </> "01-index.tar.gz" %> \path -> do
        tar <- readFileByteStringLazy (outputDir </> "01-index.tar")
        liftIO $ BSL.writeFile path (GZip.compress tar)
        putInfo $ "✅ Written " <> path

      --
      -- index cabal files
      --
      -- these come either from the package source or the revision files
      --

      outputDir </> "index/*/*/*.cabal" %> \path -> do
        let [_, _, pkgName, pkgVersion, _] = splitDirectories path
        let pkgId = PackageId pkgName pkgVersion

        meta <- getPackageMeta $ GetPackageMeta pkgId

        case latestRevisionNumber meta of
          Nothing -> do
            srcDir <- preparePackageSource $ PreparePackageSource pkgId
            copyFileChanged (srcDir </> pkgName <.> "cabal") path
          Just revNum -> do
            let revisionFile = inputDir </> pkgName </> pkgVersion </> "revisions" </> show revNum <.> "cabal"
            copyFileChanged revisionFile path

        putInfo $ "✅ Written " <> path

      --
      -- index package files (only depends on the source distribution)
      --

      outputDir </> "index/*/*/package.json" %> \path -> do
        let [_, _, pkgName, pkgVersion, _] = splitDirectories path
        let packagePath = "package" </> pkgName <> "-" <> pkgVersion <.> "tar.gz"

        let targetPath = rootPath $ fromUnrootedFilePath packagePath
        targetFileInfo <- computeFileInfoSimple' ("_repo" </> packagePath)

        expires <- getExpiryTime GetExpiryTime
        let targets =
              Targets
                { targetsVersion = FileVersion 1,
                  targetsExpires = FileExpires (Just expires),
                  targetsTargets = fromList [(TargetPathRepo targetPath, targetFileInfo)],
                  targetsDelegations = Nothing
                }

        keys <- readKeysAt (keysPath </> "target")
        let signedTargets = withSignatures hackageRepoLayout keys targets
        liftIO $ do
          p <- makeAbsolute (fromFilePath path)
          writeJSON hackageRepoLayout p signedTargets
        putInfo $ "✅ Written " <> path

      --
      -- source distributions, including patching
      --

      outputDir </> "package/*.tar.gz" %> \path -> do
        let [_, _, filename] = splitDirectories path
        let Just pkgId = parsePkgId <$> stripExtension "tar.gz" filename

        srcDir <- preparePackageSource $ PreparePackageSource pkgId
        putInfo srcDir

        withTempDir $ \tmpDir -> do
          putInfo $ "Creating source distribution for " <> pkgIdToString pkgId

          cmd_ Shell (Cwd srcDir) (FileStdout path) ("cabal sdist --ignore-project --output-directory " <> tmpDir)

          -- check cabal sdist has produced a single tarball with the
          -- expected name
          ls <- liftIO $ IO.getDirectoryContents tmpDir
          let ls' = filter (not . all (== '.')) ls
          case ls' of
            [l]
              | l == filename ->
                cmd_ Shell ["mv", tmpDir </> l, path]
            [l]
              | l /= filename ->
                fail $ "cabal sdist produced a different package. I expected " <> filename <> " but found " <> l
            _ ->
              fail $ "cabal sdist for " <> pkgIdToString pkgId <> " did not produce a single tarball!"

        putInfo $ "✅ Written " <> path

      --
      -- tarball downloads
      --

      "_cache/downloads/**" %> \path -> do
        let scheme : rest = drop 2 $ splitDirectories path
        let url = scheme <> "://" <> Posix.joinPath rest
        putInfo $ "🐢 Downloading " <> url
        cmd_ Shell (FileStdout path) $ "curl --silent -L " <> url

    putStrLn $ "💥 All done. The repository is now available in " <> outputDir <> "."

fetchUrl :: String -> Action FilePath
fetchUrl url = do
  let scheme : rest = Posix.splitPath url
      scheme' = dropWhileEnd (not . isAlpha) scheme
      urlPath = "_cache" </> "downloads" </> joinPath (scheme' : rest)
  need [urlPath]
  return urlPath

mkTarEntry :: FilePath -> [Char] -> UTCTime -> Action Tar.Entry
mkTarEntry filePath indexPath timestamp = do
  let Right tarPath = Tar.toTarPath False indexPath
  contents <- readFileByteStringLazy filePath
  return
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

applyPatches :: [Char] -> FilePath -> PackageId -> Action ()
applyPatches inputDir srcDir PackageId {pkgName, pkgVersion} = do
  let patchesDir = inputDir </> pkgName </> pkgVersion </> "patches"
  hasPatches <- doesDirectoryExist patchesDir

  when hasPatches $ do
    patchfiles <- getDirectoryFiles patchesDir ["*.patch"]
    for_ patchfiles $ \patchfile -> do
      let patch = patchesDir </> patchfile
      cmd_ Shell (Cwd srcDir) (FileStdin patch) "patch -p1"

forcePackageVersion :: FilePath -> PackageId -> Action ()
forcePackageVersion srcDir PackageId {pkgName, pkgVersion} = do
  let cabalFilePath = srcDir </> pkgName <.> "cabal"
  cabalFile <- readFile' cabalFilePath
  writeFile' cabalFilePath (replaceVersion pkgVersion cabalFile)

replaceVersion :: String -> String -> String
replaceVersion version = unlines . map f . lines
  where
    f line
      | "version" `isPrefixOf` line =
        unlines
          [ "-- version field replaced by foliage",
            "-- " <> line,
            "version: " ++ version
          ]
    f line = line
