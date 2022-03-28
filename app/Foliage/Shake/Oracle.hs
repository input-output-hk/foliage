{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.Shake.Oracle
  ( UTCTime,
    GetCurrentTime (..),
    GetExpiryTime (..),
    GetSourceMeta (..),
    GetPackages (..),
    GetSourceDir (..),
  )
where

import Data.Time.Compat ()
import Development.Shake (RuleResult)
import Development.Shake.Classes (Binary, Hashable, NFData)
import Foliage.Meta
import Foliage.Package
import GHC.Generics (Generic)

data GetCurrentTime = GetCurrentTime
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

type instance RuleResult GetCurrentTime = UTCTime

data GetExpiryTime = GetExpiryTime
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

type instance RuleResult GetExpiryTime = UTCTime

data GetPackages = GetPackages
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

type instance RuleResult GetPackages = [PackageId]

newtype GetSourceMeta = GetSourceMeta PackageId
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

type instance RuleResult GetSourceMeta = SourceMeta

newtype GetSourceDir = GetSourceDir PackageId
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

type instance RuleResult GetSourceDir = FilePath
