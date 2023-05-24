{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist
  ( prepareSdist,
    addPrepareSdistRule,
  )
where

import Control.Monad (when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import Distribution.Client.SrcDist (packageDirToSdist)
import Distribution.Package (packageId)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import Foliage.HackageSecurity
import Foliage.Meta ()
import GHC.Generics (Generic)
import Hackage.Security.Util.Path (toFilePath)
import System.Directory qualified as IO
import System.IO.Error (tryIOError)

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
    run (PrepareSdistRule srcDir) (Just old) RunDependenciesSame = do
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
          putWarn $ "Changed " ++ path ++ " (expecting hash " ++ showHashValue hvExpected ++ " found " ++ showHashValue hvExisting ++ "). I will rebuild it."
          run (PrepareSdistRule srcDir) (Just old) RunDependenciesChanged
        Left _e -> do
          putWarn $ "Unable to read " ++ path ++ ". I will rebuild it."
          run (PrepareSdistRule srcDir) (Just old) RunDependenciesChanged
    run (PrepareSdistRule srcDir) old _mode = do
      -- create the sdist distribution
      (hv, path) <- makeSdist srcDir

      let new = save (hv, path)

      let changed = case fmap ((== hv) . fst . load) old of
            Just True -> ChangedRecomputeSame
            _differentOrMissing -> ChangedRecomputeDiff

      when (changed == ChangedRecomputeSame) $
        putInfo ("Wrote " ++ path ++ " (same hash " ++ showHashValue hv ++ ")")

      when (changed == ChangedRecomputeDiff) $
        putInfo ("Wrote " ++ path ++ " (new hash " ++ showHashValue hv ++ ")")

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
        return (SHA256.hashlazy sdist, path)

    save :: (BS.ByteString, FilePath) -> BS.ByteString
    save = BSL.toStrict . Binary.encode

    load :: BS.ByteString -> (BS.ByteString, FilePath)
    load = Binary.decode . BSL.fromStrict

readFileHashValue :: FilePath -> IO BS.ByteString
readFileHashValue = fmap SHA256.hash . BS.readFile

showHashValue :: BS.ByteString -> [Char]
showHashValue = T.unpack . encodeBase16
