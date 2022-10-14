{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Meta.Aeson where

import Data.Aeson
import Data.Text
import Foliage.Meta
import Foliage.Utils.Aeson
import Network.URI (URI)

deriving via MyAesonEncoding PackageMeta instance ToJSON PackageMeta

deriving via MyAesonEncoding PackageMetaEntry instance ToJSON PackageMetaEntry

deriving via MyAesonEncoding RevisionMeta instance ToJSON RevisionMeta

deriving via MyAesonEncoding PackageVersionMeta instance ToJSON PackageVersionMeta

deriving via Text instance ToJSON GitHubRepo

deriving via Text instance ToJSON GitHubRev

instance ToJSON PackageVersionSource where
  toJSON =
    genericToJSON
      defaultOptions
        { sumEncoding = ObjectWithSingleField
        }

instance ToJSON URI where
  toJSON = toJSON . show
