{-# LANGUAGE TypeFamilies #-}

module Foliage.Oracles where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)

-- FIXME: consider using configuration variables (usingConfig/getConfig) or shakeExtra

-- | Just a shortcut to write types
type Oracle q = q -> Action (RuleResult q)

newtype CacheDir = CacheDir ()
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

type instance RuleResult CacheDir = FilePath

addCacheDirOracle :: FilePath -> Rules (Oracle CacheDir)
addCacheDirOracle inputDir =
  addOracle $ \CacheDir{} -> return inputDir
