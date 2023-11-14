{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Archive.Tar.Entry.Orphans where

import Codec.Archive.Tar.Entry
import Data.Hashable (hashUsing)
import Development.Shake.Classes
import GHC.Generics (Generic)
import System.Posix.Types (CMode)

instance Hashable CMode where
  hashWithSalt = hashUsing fromEnum

deriving instance Generic Entry
instance Hashable Entry

deriving instance Generic EntryContent
instance Hashable EntryContent

deriving instance Generic Format
instance Hashable Format

deriving instance Generic Ownership
instance Hashable Ownership

instance Hashable LinkTarget where
  hashWithSalt = hashUsing fromLinkTarget

instance Hashable TarPath where
  hashWithSalt = hashUsing fromTarPath
