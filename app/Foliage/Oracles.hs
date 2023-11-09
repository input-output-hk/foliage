{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.Oracles where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Hackage.Security.Client (
  RepoLayout,
  RepoPath,
  anchorRepoPathLocally,
  hackageRepoLayout,
 )
import Hackage.Security.Util.Path qualified as Sec
import Hackage.Security.Util.Pretty qualified as Sec

data InputRoot

type InputPath = Sec.Path InputRoot

instance Sec.Pretty InputPath where
  pretty (Sec.Path fp) = "<input>/" ++ fp

data InputDir = InputDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult InputDir = FilePath

addInputDirOracle :: FilePath -> Rules (InputDir -> Action FilePath)
addInputDirOracle inputDir =
  addOracle $ \InputDir -> return inputDir

anchorInputPath :: InputPath -> Action (Sec.Path Sec.Absolute)
anchorInputPath ip = do
  inputDir <- askOracle InputDir
  inputDirRoot <- liftIO $ Sec.makeAbsolute (Sec.fromFilePath inputDir)
  return $ inputDirRoot Sec.</> Sec.unrootPath ip

data CacheRoot

type CachePath = Sec.Path CacheRoot

instance Sec.Pretty CachePath where
  pretty (Sec.Path fp) = "<cache>/" ++ fp

data CacheDir = CacheDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult CacheDir = FilePath

addCacheDirOracle :: FilePath -> Rules (CacheDir -> Action FilePath)
addCacheDirOracle inputDir =
  addOracle $ \CacheDir -> return inputDir

anchorCachePath :: CachePath -> Action (Sec.Path Sec.Absolute)
anchorCachePath ip = do
  cacheDir <- askOracle CacheDir
  cacheDirRoot <- liftIO $ Sec.makeAbsolute (Sec.fromFilePath cacheDir)
  return $ cacheDirRoot Sec.</> Sec.unrootPath ip

data OutputDir = OutputDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OutputDir = FilePath

getOutputDir :: Action (Sec.Path Sec.Absolute)
getOutputDir = do
  outputDirRoot <- askOracle OutputDir
  liftIO $ Sec.makeAbsolute (Sec.fromFilePath outputDirRoot)

anchorRepoPath :: RepoPath -> Action (Sec.Path Sec.Absolute)
anchorRepoPath rp = do
  outputDir <- getOutputDir
  return $ anchorRepoPathLocally outputDir rp

anchorRepoPath' :: (RepoLayout -> RepoPath) -> Action (Sec.Path Sec.Absolute)
anchorRepoPath' p = do
  outputDir <- getOutputDir
  return $ anchorRepoPathLocally outputDir $ p hackageRepoLayout

addOutputDirOracle :: FilePath -> Rules (OutputDir -> Action FilePath)
addOutputDirOracle outputDir =
  addOracle $ \OutputDir -> return outputDir
