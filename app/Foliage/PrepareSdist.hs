{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.PrepareSdist (
  prepareSdist,
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
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity qualified as Verbosity
import Foliage.Meta ()
import GHC.Generics (Generic)
import System.Directory qualified as IO
import System.IO.Error (tryIOError)

data PrepareSdistRule = PrepareSdistRule FilePath FilePath
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult PrepareSdistRule = ()

prepareSdist :: FilePath -> FilePath -> Action ()
prepareSdist srcDir dstPath = apply1 $ PrepareSdistRule srcDir dstPath

addPrepareSdistRule :: Rules ()
addPrepareSdistRule = addBuiltinRule noLint noIdentity run
 where
  run (PrepareSdistRule srcDir dstPath) (Just old) RunDependenciesSame = do
    let hvExpected = load old

    -- Check of has of the sdist, if the sdist is still there and it is
    -- indeed what we expect, signal that nothing changed. Otherwise
    -- warn the user and proceed to recompute.
    ehvExisting <- liftIO $ tryIOError $ readFileHashValue dstPath
    case ehvExisting of
      Right hvExisting
        | hvExisting == hvExpected ->
            return
              RunResult
                { runChanged = ChangedNothing
                , runStore = old
                , runValue = ()
                }
      Right hvExisting -> do
        putWarn $ "Changed " ++ dstPath ++ " (expecting hash " ++ showHashValue hvExpected ++ " found " ++ showHashValue hvExisting ++ "). I will rebuild it."
        run (PrepareSdistRule srcDir dstPath) (Just old) RunDependenciesChanged
      Left _ -> do
        putWarn $ "Unable to read " ++ dstPath ++ ". I will rebuild it."
        run (PrepareSdistRule srcDir dstPath) (Just old) RunDependenciesChanged
  run (PrepareSdistRule srcDir dstPath) old _mode = do
    -- create the sdist distribution
    hv <- makeSdist srcDir dstPath

    let new = save hv

    let changed = case fmap ((== hv) . load) old of
          Just True -> ChangedRecomputeSame
          _differentOrMissing -> ChangedRecomputeDiff

    when (changed == ChangedRecomputeSame) $
      putInfo ("Wrote " ++ dstPath ++ " (same hash " ++ showHashValue hv ++ ")")

    when (changed == ChangedRecomputeDiff) $
      putInfo ("Wrote " ++ dstPath ++ " (new hash " ++ showHashValue hv ++ ")")

    return $ RunResult{runChanged = changed, runStore = new, runValue = ()}

  makeSdist srcDir dstPath = do
    cabalFiles <- getDirectoryFiles srcDir ["*.cabal"]
    let cabalFile = case cabalFiles of
          [f] -> f
          fs ->
            error $
              unlines
                [ "Invalid source directory: " ++ srcDir
                , "It contains multiple cabal files, while only one is allowed"
                , unwords fs
                ]

    need [srcDir </> cabalFile]

    gpd <- liftIO $ readGenericPackageDescription Verbosity.normal (srcDir </> cabalFile)

    traced "cabal sdist" $ do
      IO.createDirectoryIfMissing True (takeDirectory dstPath)
      sdist <- packageDirToSdist Verbosity.normal gpd srcDir
      BSL.writeFile dstPath sdist
      return $ SHA256.hashlazy sdist

  save = BSL.toStrict . Binary.encode
  load = Binary.decode . BSL.fromStrict

readFileHashValue :: FilePath -> IO BS.ByteString
readFileHashValue = fmap SHA256.hash . BS.readFile

showHashValue :: BS.ByteString -> [Char]
showHashValue = T.unpack . encodeBase16
