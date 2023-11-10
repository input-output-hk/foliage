{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist (
  prepareSdist,
  -- addPrepareSdistRule,
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
import Distribution.Client.Utils (tryFindPackageDesc)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.SrcDist (listPackageSourcesWithDie)
import Distribution.Simple.Utils (findPackageDesc)
import Distribution.Verbosity qualified as Verbosity
import Foliage.Meta ()
import GHC.Generics (Generic)
import System.Directory qualified as IO
import System.IO.Error (tryIOError)

data PrepareSdistRule = PrepareSdistRule GenericPackageDescription FilePath FilePath
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, NFData)

instance Hashable PrepareSdistRule where
  hashWithSalt s (PrepareSdistRule gpd srcDir dstPath) =
    s `hashWithSalt` show gpd `hashWithSalt` srcDir `hashWithSalt` dstPath

type instance RuleResult PrepareSdistRule = ()

prepareSdist :: GenericPackageDescription -> FilePath -> FilePath -> Action ()
-- prepareSdist gpd srcDir dstPath = apply1 $ PrepareSdistRule gpd srcDir dstPath
prepareSdist = makeSdist

-- addPrepareSdistRule :: Rules ()
-- addPrepareSdistRule = addBuiltinRule noLint noIdentity run
--  where
--   run (PrepareSdistRule gpd srcDir dstPath) (Just old) RunDependenciesSame = do
--     let hvExpected = load old
--
--     -- Check of has of the sdist, if the sdist is still there and it is
--     -- indeed what we expect, signal that nothing changed. Otherwise
--     -- warn the user and proceed to recompute.
--     ehvExisting <- liftIO $ tryIOError $ readFileHashValue dstPath
--     case ehvExisting of
--       Right hvExisting
--         | hvExisting == hvExpected ->
--             return
--               RunResult
--                 { runChanged = ChangedNothing
--                 , runStore = old
--                 , runValue = ()
--                 }
--       Right hvExisting -> do
--         putWarn $ "Changed " ++ dstPath ++ " (expecting hash " ++ showHashValue hvExpected ++ " found " ++ showHashValue hvExisting ++ "). I will rebuild it."
--         run (PrepareSdistRule gpd srcDir dstPath) (Just old) RunDependenciesChanged
--       Left _ -> do
--         putWarn $ "Unable to read " ++ dstPath ++ ". I will rebuild it."
--         run (PrepareSdistRule gpd srcDir dstPath) (Just old) RunDependenciesChanged
--   run (PrepareSdistRule gpd srcDir dstPath) old _mode = do
--     -- create the sdist distribution
--     hv <- makeSdist gpd srcDir dstPath
--
--     let changed = case fmap load old of
--           Just hv' | hv' == hv -> ChangedRecomputeSame
--           _differentOrMissing -> ChangedRecomputeDiff
--
--     when (changed == ChangedRecomputeSame) $
--       putInfo $
--         "Wrote " ++ dstPath ++ " (same hash " ++ showHashValue hv ++ ")"
--
--     when (changed == ChangedRecomputeDiff) $
--       putInfo $
--         "Wrote " ++ dstPath ++ " (new hash " ++ showHashValue hv ++ ")"
--
--     return $ RunResult{runChanged = changed, runStore = save hv, runValue = ()}
--
--   save = BSL.toStrict . Binary.encode
--   load = Binary.decode . BSL.fromStrict

makeSdist :: GenericPackageDescription -> [Char] -> FilePath -> Action ()
makeSdist gpd srcDir dstPath = do
  produces [dstPath]

  liftIO $ do
    IO.createDirectoryIfMissing True (takeDirectory dstPath)
    sdist <- packageDirToSdist Verbosity.normal gpd srcDir
    BSL.writeFile dstPath sdist

readFileHashValue :: FilePath -> IO BS.ByteString
readFileHashValue = fmap SHA256.hash . BS.readFile

showHashValue :: BS.ByteString -> [Char]
showHashValue = T.unpack . encodeBase16

assertSingle :: ([a] -> [Char]) -> [a] -> a
assertSingle _err [f] = f
assertSingle err fs = error (err fs)

-- | Adapted from cabal-install, we already have the GenericPackageDescription
allPackageSourceFiles :: Verbosity.Verbosity -> FilePath -> GenericPackageDescription -> IO [FilePath]
allPackageSourceFiles verbosity packageDir gpd = do
  listPackageSourcesWithDie
    verbosity
    (\_verbosity' err -> fail err)
    packageDir
    (flattenPackageDescription gpd)
    knownSuffixHandlers
