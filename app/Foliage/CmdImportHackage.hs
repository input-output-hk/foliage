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
import Distribution.Package (PackageIdentifier (PackageIdentifier, pkgVersion), pkgName)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageName (unPackageName)
import Foliage.Meta
import Foliage.Options
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath

cmdImportHackage :: ImportHackageOptions -> IO ()
cmdImportHackage (ImportHackageOptions Nothing) = importHackage (const True)
cmdImportHackage (ImportHackageOptions (Just f)) = importHackage (mkFilter f)
  where
    mkFilter (ImportFilter pn Nothing) = (== pn) . unPackageName . pkgName
    mkFilter (ImportFilter pn (Just pv)) = (&&) <$> (== pn) . unPackageName . pkgName <*> (== pv) . prettyShow . pkgVersion

importHackage ::
  (PackageIdentifier -> Bool) ->
  IO ()
importHackage f = do
  putStrLn "EXPERIMENTAL. Import the Hackage index from $HOME/.cabal. Make sure you have done `cabal update` recently."
  home <- getEnv "HOME"
  entries <- Tar.read <$> BSL.readFile (home </> ".cabal/packages/hackage.haskell.org/01-index.tar")
  m <- importIndex f entries M.empty
  for_ (M.toList m) $ uncurry finalise

importIndex ::
  Show e =>
  (PackageIdentifier -> Bool) ->
  Tar.Entries e ->
  Map PackageIdentifier PackageVersionMeta ->
  IO (Map PackageIdentifier PackageVersionMeta)
importIndex f (Tar.Next e es) m =
  case isCabalFile e of
    Just (pkgId, contents, time)
      | f pkgId ->
        do
          putStrLn $ "Found cabal file " ++ prettyShow pkgId ++ " with timestamp " ++ show time
          let -- new package
              go Nothing =
                pure $
                  Just $
                    PackageVersionMeta
                      { packageVersionSource = TarballSource (pkgIdToHackageUrl pkgId) Nothing,
                        packageVersionTimestamp = Just time,
                        packageVersionRevisions = [],
                        packageVersionForce = False
                      }
              -- Existing package, new revision
              go (Just sm) = do
                let revnum = 1 + fromMaybe 0 (latestRevisionNumber sm)
                    newRevision = RevisionMeta {revisionNumber = revnum, revisionTimestamp = time}
                -- Repeatedly adding at the end of a list is bad performance but good for the moment.
                let sm' = sm {packageVersionRevisions = packageVersionRevisions sm ++ [newRevision]}
                let PackageIdentifier pkgName pkgVersion = pkgId
                let outDir = "_sources" </> unPackageName pkgName </> prettyShow pkgVersion </> "revisions"
                createDirectoryIfMissing True outDir
                BSL.writeFile (outDir </> show revnum <.> "cabal") contents
                return $ Just sm'
          m' <- M.alterF go pkgId m
          importIndex f es m'
    _ -> importIndex f es m
importIndex _f Tar.Done m =
  return m
importIndex _f (Tar.Fail e) _ =
  error $ show e

pkgIdToHackageUrl :: PackageIdentifier -> String
pkgIdToHackageUrl pkgId =
  "https://hackage.haskell.org/package" </> prettyShow pkgId </> prettyShow pkgId <.> "tar.gz"

finalise ::
  PackageIdentifier ->
  PackageVersionMeta ->
  IO ()
finalise PackageIdentifier {pkgName, pkgVersion} meta = do
  let dir = "_sources" </> unPackageName pkgName </> prettyShow pkgVersion
  createDirectoryIfMissing True dir
  writePackageVersionMeta (dir </> "meta.toml") meta

isCabalFile ::
  Tar.Entry ->
  Maybe (PackageIdentifier, BSL.ByteString, UTCTime)
isCabalFile
  Tar.Entry
    { Tar.entryTarPath = Tar.fromTarPath -> path,
      Tar.entryContent = Tar.NormalFile contents _,
      Tar.entryTime = posixSecondsToUTCTime . fromIntegral -> time
    }
    | ".cabal" `isSuffixOf` path =
      let [pkgName, pkgVersion, _] = splitDirectories path
          Just name = simpleParsec pkgName
          Just version = simpleParsec pkgVersion
          packageId = PackageIdentifier name version
       in Just (packageId, contents, time)
isCabalFile _ = Nothing
