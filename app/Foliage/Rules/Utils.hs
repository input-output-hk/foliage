{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.Rules.Utils where

import Data.List.NonEmpty qualified as NE
import Data.Map
import Data.Map.Strict qualified as M
import Development.Shake
import Development.Shake.Classes
import Distribution.Package (PackageId)
import Distribution.PackageDescription (PackageIdentifier (..), PackageName)
import Distribution.Version (Version)
import Foliage.Meta
import GHC.Generics (Generic)

groupByPackageName :: Map PackageId (FilePath, PackageVersionSpec) -> Map PackageName (NE.NonEmpty (Version, PackageVersionSpec))
groupByPackageName pkgSpecs =
  M.fromListWith
    (<>)
    [ (pkgName pkgId, NE.singleton (pkgVersion pkgId, pkgSpec))
    | (pkgId, (_metaFile, pkgSpec)) <- M.toList pkgSpecs
    ]

newtype PkgSpecFor = PkgSpecFor PackageId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult PkgSpecFor = PackageVersionSpec

data PkgSpecs = PkgSpecs
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult PkgSpecs = Map PackageId (FilePath, PackageVersionSpec)
