{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Foliage.PrepareSdist
  ( prepareSdist,
    addPrepareSdistRule,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Development.Shake
import Development.Shake.FilePath (takeDirectory, (<.>), (</>))
import Development.Shake.Rule
import Distribution.Client.HashValue (readFileHashValue)
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Compat.Binary (encode)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta
import Foliage.PrepareSource (prepareSource)
import Foliage.Shake
import Hackage.Security.Util.Path (toFilePath)
import System.Directory qualified as IO

prepareSdist :: PackageId -> PackageVersionMeta -> Action FilePath
prepareSdist pkgId pkgMeta = apply1 $ PackageRule @"prepareSdist" pkgId pkgMeta

addPrepareSdistRule :: Path Absolute -> Rules ()
addPrepareSdistRule outputDirRoot = addBuiltinRule noLint noIdentity run
  where
    run :: PackageRule "prepareSdist" FilePath -> Maybe BS.ByteString -> RunMode -> Action (RunResult FilePath)
    run (PackageRule pkgId _pkgMeta) (Just old) RunDependenciesSame =
      let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
          path = toFilePath $ anchorRepoPathLocally outputDirRoot packagePath
       in return $ RunResult ChangedNothing old path
    run (PackageRule pkgId pkgMeta) old _ = do
      let packagePath = repoLayoutPkgTarGz hackageRepoLayout pkgId
          path = toFilePath $ anchorRepoPathLocally outputDirRoot packagePath
      srcDir <- prepareSource pkgId pkgMeta
      let PackageIdentifier {pkgName} = pkgId
      traced "cabal sdist" $ do
        let cabalFilePath = srcDir </> unPackageName pkgName <.> "cabal"
        gpd <- readGenericPackageDescription Verbosity.normal cabalFilePath
        IO.createDirectoryIfMissing True (takeDirectory path)
        packageDirToSdist Verbosity.normal gpd srcDir
          >>= BSL.writeFile path
        hash <- BSL.toStrict . encode <$> readFileHashValue path
        return $
          if old == Just hash
            then RunResult ChangedRecomputeSame hash path
            else RunResult ChangedRecomputeDiff hash path
