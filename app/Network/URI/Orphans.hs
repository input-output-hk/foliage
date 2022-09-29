{-# OPTIONS_GHC -Wno-orphans #-}

module Network.URI.Orphans where

import Development.Shake.Classes
import Network.URI

instance Binary URIAuth

instance Binary URI

instance Hashable URIAuth

instance Hashable URI
