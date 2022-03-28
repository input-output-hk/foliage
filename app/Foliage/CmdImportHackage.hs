module Foliage.CmdImportHackage
  ( cmdImportHackage,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foliage.Meta
import Foliage.Options
import Foliage.Package
import System.Directory qualified as IO
import System.Environment
import System.FilePath

cmdImportHackage :: ImportHackageOptions -> IO ()
cmdImportHackage (ImportHackageOptions Nothing) = importHackage (const True)
cmdImportHackage (ImportHackageOptions (Just s)) = importHackage ((== s) . pkgName)

importHackage ::
  (PackageId -> Bool) ->
  IO ()
importHackage f = do
  putStrLn "EXPERIMENTAL. Import the Hackage index from $HOME/.cabal. Make sure you have done `cabal update` recently."
  home <- getEnv "HOME"
  entries <- Tar.read <$> BSL.readFile (home </> ".cabal/packages/hackage.haskell.org/01-index.tar")
  m <- importIndex f entries M.empty
  for_ (M.toList m) $ uncurry finalise

importIndex ::
  Show e =>
  (PackageId -> Bool) ->
  Tar.Entries e ->
  Map PackageId SourceMeta ->
  IO (Map PackageId SourceMeta)
importIndex f (Tar.Next e es) m =
  case isCabalFile e of
    Just (pkgId, contents, time)
      | f pkgId ->
        do
          putStrLn $ "Found cabal file " ++ pkgIdToString pkgId ++ " with time " ++ show time
          m' <-
            M.alterF
              ( \case
                  -- New package
                  Nothing ->
                    pure $
                      Just $
                        SourceMeta
                          { sourceUrl = pkgIdToHackageUrl pkgId,
                            sourceTimestamp = time,
                            sourceSubdir = Nothing,
                            sourceRevisions = []
                          }
                  -- Existing package, new revision
                  Just sm -> do
                    let revnum = 1 + fromMaybe 0 (latestRevisionNumber sm)
                        newRevision = RevisionMeta {revisionNumber = revnum, revisionTimestamp = time}
                    -- bad performance here but I don't care
                    let sm' = sm {sourceRevisions = sourceRevisions sm ++ [newRevision]}
                    let PackageId pkgName pkgVersion = pkgId
                    let outDir = "_sources" </> pkgName </> pkgVersion </> "revisions"
                    IO.createDirectoryIfMissing True outDir
                    BSL.writeFile (outDir </> show revnum <.> "cabal") contents
                    return $ Just sm'
              )
              pkgId
              m
          importIndex f es m'
    _ -> importIndex f es m
importIndex _f Tar.Done m =
  return m
importIndex _f (Tar.Fail e) _ =
  error $ show e

finalise ::
  PackageId ->
  SourceMeta ->
  IO ()
finalise PackageId {pkgName, pkgVersion} meta = do
  let dir = "_sources" </> pkgName </> pkgVersion
  IO.createDirectoryIfMissing True dir
  writeSourceMeta (dir </> "meta.toml") meta

isCabalFile ::
  Tar.Entry ->
  Maybe (PackageId, BSL.ByteString, UTCTime)
isCabalFile
  Tar.Entry
    { Tar.entryTarPath = Tar.fromTarPath -> path,
      Tar.entryContent = Tar.NormalFile contents _,
      Tar.entryTime = posixSecondsToUTCTime . fromIntegral -> time
    }
    | ".cabal" `isSuffixOf` path =
      let [pkgName, pkgVersion, _] = splitDirectories path
          packageId = PackageId pkgName pkgVersion
       in Just (packageId, contents, time)
isCabalFile _ = Nothing
