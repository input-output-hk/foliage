{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.Shake
  ( computeFileInfoSimple',
    readKeysAt,
    readPackageVersionMeta',
    PackageRule (PackageRule),
  )
where

import Data.Traversable (for)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Package (PackageId)
import Foliage.HackageSecurity
import Foliage.Meta
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)

computeFileInfoSimple' :: FilePath -> Action FileInfo
computeFileInfoSimple' fp = do
  need [fp]
  liftIO $ computeFileInfoSimple fp

readKeysAt :: FilePath -> Action [Some Key]
readKeysAt base = do
  paths <- getDirectoryFiles base ["*.json"]
  need $ map (base </>) paths
  for paths $ \path -> do
    Right key <- liftIO $ readJSONSimple (base </> path)
    pure key

readPackageVersionMeta' :: FilePath -> Action PackageVersionMeta
readPackageVersionMeta' fp = do
  need [fp]
  liftIO $ readPackageVersionMeta fp

data PackageRule (tag :: Symbol) a = PackageRule PackageId PackageVersionMeta
  deriving (Show, Eq, Generic)
  deriving (Hashable, Binary, NFData)

type instance RuleResult (PackageRule tag a) = a
