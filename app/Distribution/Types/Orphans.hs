{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Types.Orphans where

import Development.Shake.Classes (Hashable)
import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Utils.ShortText (ShortText)

instance Hashable ShortText

instance Hashable PackageName

instance Hashable PackageIdentifier

instance Hashable Version

instance Hashable VersionRange
