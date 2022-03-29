{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Foliage.CmdBuild (cmdBuild) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.List (isPrefixOf, sortOn)
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
import Foliage.Utils
import System.Directory qualified as IO
import Data.Maybe (fromMaybe)

cmdBuild :: BuildOptions -> IO ()
cmdBuild (BuildOptions keysPath mCurrentTime outDir) = do
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

    getSourceMeta <- addOracle $ \(GetSourceMeta PackageId {pkgName, pkgVersion}) ->
      readSourceMeta' $ "_sources" </> pkgName </> pkgVersion </> "meta.toml"

    getSourceDir <- addOracle $ \(GetSourceDir pkgId) -> do
      SourceMeta {sourceUrl, sourceSubdir} <- getSourceMeta (GetSourceMeta pkgId)
      let srcDir = "_cache" </> urlToFileName sourceUrl

      need [srcDir </> ".downloaded"]
      -- FIXME Without this, sometimes the download doesn't trigger
      putInfo $ "👀 " <> sourceUrl

      projectFiles <- liftIO $ filter ("cabal.project" `isPrefixOf`) <$> IO.getDirectoryContents srcDir
      unless (null projectFiles) $ do
        putWarn $ "⚠️ Deleting cabal project files from " ++ srcDir
        liftIO $ for_ projectFiles $ IO.removeFile . (srcDir </>)

      return $ case sourceSubdir of
        Just s -> srcDir </> s
        Nothing -> srcDir

    getPackages <- addOracle $ \GetPackages -> do
      metaFiles <- getDirectoryFiles "_sources" ["*/*/meta.toml"]
      return $
        [ PackageId pkgName pkgVersion
          | path <- metaFiles,
            let [pkgName, pkgVersion, _] = splitDirectories path
        ]

    --
    -- Entrypoint
    --

    -- This triggers the whole chain of TUF metadata
    want [outDir </> "timestamp.json"]

    -- This build the current index entry for all packages
    action $ do
      pkgIds <- getPackages GetPackages
      need
        [ outDir </> "index" </> pkgName </> pkgVersion </> pkgName <.> "cabal"
          | PackageId pkgName pkgVersion <- pkgIds
        ]

    --
    -- timestamp.json
    --
    outDir </> "timestamp.json" %> \path -> do
      snapshotInfo <- computeFileInfoSimple' (outDir </> "snapshot.json")
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

    outDir </> "snapshot.json" %> \path -> do
      rootInfo <- computeFileInfoSimple' (outDir </> "root.json")
      mirrorsInfo <- computeFileInfoSimple' (outDir </> "mirrors.json")
      tarInfo <- computeFileInfoSimple' (outDir </> "01-index.tar")
      tarGzInfo <- computeFileInfoSimple' (outDir </> "01-index.tar.gz")
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

    outDir </> "root.json" %> \path -> do
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

    outDir </> "mirrors.json" %> \path -> do
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

    outDir </> "01-index.tar" %> \path -> do
      pkgIds <- getPackages GetPackages

      entries <-
        fmap concat $
          for pkgIds $ \pkgId -> do
            let PackageId {pkgName, pkgVersion} = pkgId
            SourceMeta {sourceTimestamp, sourceRevisions} <- getSourceMeta (GetSourceMeta pkgId)

            srcDir <- getSourceDir (GetSourceDir pkgId)
            now <- getCurrentTime GetCurrentTime

            sequence $
              [ -- original cabal file
                mkTarEntry
                  (srcDir </> pkgName <.> "cabal")
                  (pkgName </> pkgVersion </> pkgName <.> "cabal")
                  (fromMaybe now sourceTimestamp),
                -- package.json
                mkTarEntry
                  (outDir </> "index" </> pkgName </> pkgVersion </> "package.json")
                  (pkgName </> pkgVersion </> "package.json")
                  (fromMaybe now sourceTimestamp)
              ]
                ++ [ -- revised cabal files
                     mkTarEntry
                       ("_sources" </> pkgName </> pkgVersion </> "revisions" </> show revNum <.> "cabal")
                       (pkgName </> pkgVersion </> pkgName <.> "cabal")
                       (fromMaybe now revTimestamp)
                     | RevisionMeta revTimestamp revNum <- sourceRevisions
                   ]

      liftIO $ BSL.writeFile path $ Tar.write (sortOn Tar.entryTime entries)
      putInfo $ "✅ Written " <> path

    --
    -- 01-index.tar.gz
    --

    outDir </> "01-index.tar.gz" %> \path -> do
      tar <- readFileByteStringLazy (outDir </> "01-index.tar")
      liftIO $ BSL.writeFile path (GZip.compress tar)
      putInfo $ "✅ Written " <> path

    --
    -- index cabal files (latest revision)
    --

    outDir </> "index/*/*/*.cabal" %> \path -> do
      let [_, _, pkgName, pkgVersion, _] = splitDirectories path
      let pkgId = PackageId pkgName pkgVersion

      -- Figure out where to get it from
      meta <- getSourceMeta $ GetSourceMeta pkgId

      case latestRevisionNumber meta of
        Nothing -> do
          srcDir <- getSourceDir (GetSourceDir pkgId)
          copyFileChanged (srcDir </> pkgName <.> "cabal") path
        Just revNum -> do
          let revisionCabal = "_sources" </> pkgName </> pkgVersion </> "revisions" </> show revNum <.> "cabal"
          copyFileChanged revisionCabal path

      putInfo $ "✅ Written " <> path

    --
    -- index package files (only depends on the source distribution)
    --

    outDir </> "index/*/*/package.json" %> \path -> do
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
    -- source distributions
    --

    outDir </> "package/*.tar.gz" %> \path -> do
      let [_, _, filename] = splitDirectories path
      let Just pkgId = parsePkgId <$> stripExtension "tar.gz" filename

      srcDir <- getSourceDir (GetSourceDir pkgId)

      withTempDir $ \tmpDir -> do
        putInfo $ " Creating source distribution for " <> pkgIdToString pkgId
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
    -- source tree downloads
    --

    "_cache/*/.downloaded" %> \path -> do
      let [_, hashedUrl, _] = splitDirectories path
      let url = fileNameToUrl hashedUrl
      let srcDir = takeDirectory path

      withTempDir $ \tmpDir -> do
        -- Download and extract tarball
        putInfo $ "🐢 Downloading " <> url
        cmd_ Shell $ "curl --silent -L " <> url <> " | tar xz -C " <> tmpDir

        -- Special treatment of top-level directory: we remove it
        --
        -- Note: Don't let shake look into tmpDir! it will cause
        -- unnecessary rework because tmpDir is always new
        ls <- liftIO $ IO.getDirectoryContents tmpDir
        let ls' = filter (not . all (== '.')) ls
        case ls' of
          [l] -> cmd_ Shell ["mv", "-T", tmpDir </> l, srcDir]
          _ -> cmd_ Shell ["mv", "-T", tmpDir, srcDir]

      -- Touch the trigger file
      writeFile' path ""

  putStrLn $ "💥 All done. The repository is now available in " <> outDir <> "."

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
