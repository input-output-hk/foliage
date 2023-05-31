{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist
  ( prepareSdist,
    addPrepareSdistRule,
  )
where

import Control.Monad (when)
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Package (PackageId, packageId)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Orphans ()
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta.Hash
import GHC.Generics (Generic)
import Hackage.Security.Util.Path (toFilePath)
import System.Directory qualified as IO
import System.IO.Error (tryIOError)

-- newtype SDist = SDist PackageId
--   deriving (Show, Eq, Generic)
--   deriving newtype (Hashable, Binary, NFData)
--
-- type instance RuleResult SDist = ()
--
-- data SDistRule = SDistRule SDist (Action ())
--
-- sdistRule :: PackageId -> Action () -> Rules ()
-- sdistRule pkgId act = addUserRule $ SDistRule (SDist pkgId) act
--
-- sdistNeed :: PackageId -> Action ()
-- sdistNeed = apply1 . SDist
--
data PrepareSdistRule = PrepareSdistRule FilePath (Maybe SHA256)
  deriving (Show, Eq, Generic)
  deriving (Hashable, Binary, NFData)

type instance RuleResult PrepareSdistRule = FilePath

prepareSdist :: FilePath -> Maybe SHA256 -> Action FilePath
prepareSdist srcDir mHash = apply1 $ PrepareSdistRule srcDir mHash

addPrepareSdistRule :: Path Absolute -> Rules ()
addPrepareSdistRule outputDirRoot = addBuiltinRule noLint noIdentity run
  where
    run :: PrepareSdistRule -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
    run (PrepareSdistRule srcDir mHash) (Just old) RunDependenciesSame = do
      let (hvExpected, path) = load old

      -- Check of has of the sdist, if the sdist is still there and it is
      -- indeed what we expect, signal that nothing changed. Otherwise
      -- warn the user and proceed to recompute.
      ehvExisting <- liftIO $ tryIOError $ readFileHashValue path
      case ehvExisting of
        Right hvExisting
          | hvExisting == hvExpected ->
              return RunResult {runChanged = ChangedNothing, runStore = old, runValue = path}
        Right hvExisting -> do
          putWarn $ "Changed " ++ path ++ " (expecting hash " ++ show hvExpected ++ " found " ++ show hvExisting ++ "). I will rebuild it."
          run (PrepareSdistRule srcDir mHash) (Just old) RunDependenciesChanged
        Left _e -> do
          putWarn $ "Unable to read " ++ path ++ ". I will rebuild it."
          run (PrepareSdistRule srcDir mHash) (Just old) RunDependenciesChanged
    run (PrepareSdistRule srcDir mHash) old _mode = do
      -- create the sdist distribution
      (hv, path) <- makeSdist srcDir

      let new = save (hv, path)

      let changed = case fmap ((== hv) . fst . load) old of
            Just True -> ChangedRecomputeSame
            _differentOrMissing -> ChangedRecomputeDiff

      when (changed == ChangedRecomputeSame) $
        putInfo ("Wrote " ++ path ++ " (same hash " ++ show hv ++ ")")

      when (changed == ChangedRecomputeDiff) $
        putInfo ("Wrote " ++ path ++ " (new hash " ++ show hv ++ ")")

      return $ RunResult {runChanged = changed, runStore = new, runValue = path}

    makeSdist srcDir = do
      cabalFiles <- getDirectoryFiles srcDir ["*.cabal"]
      let cabalFile = case cabalFiles of
            [f] -> f
            fs ->
              error $
                unlines
                  [ "Invalid source directory: " ++ srcDir,
                    "It contains multiple cabal files, while only one is allowed",
                    unwords fs
                  ]

      traced "cabal sdist" $ do
        gpd <- readGenericPackageDescription Verbosity.normal (srcDir </> cabalFile)
        let pkgId = packageId gpd
            packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
            path = toFilePath $ anchorRepoPathLocally outputDirRoot packagePath
        IO.createDirectoryIfMissing True (takeDirectory path)
        sdist <- packageDirToSdist Verbosity.normal gpd srcDir
        BSL.writeFile path sdist
        return (hashlazy sdist, path)

    save :: (SHA256, FilePath) -> BS.ByteString
    save = BSL.toStrict . Binary.encode

    load :: BS.ByteString -> (SHA256, FilePath)
    load = Binary.decode . BSL.fromStrict
