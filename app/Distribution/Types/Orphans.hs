{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Types.Orphans where

import Development.Shake.Classes (Hashable)
import Distribution.Types.Version
import Distribution.Types.VersionRange

instance Hashable Version

instance Hashable VersionRange
