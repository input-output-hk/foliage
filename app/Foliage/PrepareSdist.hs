{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist
  ( prepareSdist,
    addPrepareSdistRule,
  )
where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Distribution.Client.HashValue (HashValue, hashValue, showHashValue)
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Compat.Binary (decode, encode)
import Distribution.Package (packageId)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta ()
import GHC.Generics (Generic)
import Hackage.Security.Util.Path (toFilePath)
import System.Directory qualified as IO

newtype PrepareSdistRule = PrepareSdistRule FilePath
  deriving (Show, Eq, Generic)
  deriving (Hashable, Binary, NFData)

type instance RuleResult PrepareSdistRule = FilePath

prepareSdist :: FilePath -> Action FilePath
prepareSdist srcDir = apply1 $ PrepareSdistRule srcDir

addPrepareSdistRule :: Path Absolute -> Rules ()
addPrepareSdistRule outputDirRoot = addBuiltinRule noLint noIdentity run
  where
    run :: PrepareSdistRule -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
    run (PrepareSdistRule _srcDir) (Just old) RunDependenciesSame =
      let (_, path) = load old
       in return $ RunResult ChangedNothing old path
    run (PrepareSdistRule srcDir) old _ = do
      cabalFile <- do
        getDirectoryFiles srcDir ["*.cabal"] >>= \case
          [f] -> pure f
          fs -> fail $ "Invalid srcDir: " ++ srcDir ++ ". It contains multiple cabal files: " ++ unwords fs
      (hv, path) <- traced "cabal sdist" $ do
        gpd <- readGenericPackageDescription Verbosity.normal (srcDir </> cabalFile)
        let pkgId = packageId gpd
            packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
            path = toFilePath $ anchorRepoPathLocally outputDirRoot packagePath
        IO.createDirectoryIfMissing True (takeDirectory path)
        sdist <- packageDirToSdist Verbosity.normal gpd srcDir
        BSL.writeFile path sdist
        return (hashValue sdist, path)
      let new = save (hv, path)

      let changed = case old of
            Just old' | fst (load old') == hv -> ChangedRecomputeSame
            _ -> ChangedRecomputeDiff

      when (changed == ChangedRecomputeSame) $
        putInfo $ "Wrote " ++ path ++ " (same hash " ++ showHashValue hv ++ ")"

      when (changed == ChangedRecomputeDiff) $
        putInfo $ "Wrote " ++ path ++ " (new hash " ++ showHashValue hv ++ ")"

      return $ RunResult {runChanged = changed, runStore = new, runValue = path}

    save :: (HashValue, FilePath) -> BS.ByteString
    save = BSL.toStrict . encode

    load :: BS.ByteString -> (HashValue, FilePath)
    load = decode . BSL.fromStrict
