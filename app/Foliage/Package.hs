{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Foliage.Package
  ( PackageId (..),
    parsePkgId,
    pkgIdToTarGzName,
    pkgIdToString,
    pkgIdToHackageUrl,
  )
where

import Data.Bifunctor
import Data.Tuple
import Development.Shake.Classes
import GHC.Generics
import System.FilePath

data PackageId = PackageId {pkgName :: String, pkgVersion :: String}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, NFData, Hashable)

parsePkgId :: String -> PackageId
parsePkgId fn = PackageId (init pn) pv
  where
    (pn, pv) = swap $ bimap reverse reverse $ break (== '-') $ reverse fn

pkgIdToTarGzName :: PackageId -> FilePath
pkgIdToTarGzName pkgId = pkgIdToString pkgId <.> "tar.gz"

pkgIdToString :: PackageId -> String
pkgIdToString (PackageId name version) = name <> "-" <> version

pkgIdToHackageUrl :: PackageId -> String
pkgIdToHackageUrl pkgId =
  "https://hackage.haskell.org/package" </> pkgIdToString pkgId </> pkgIdToString pkgId <.> "tar.gz"
