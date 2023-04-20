{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Meta.Aeson where

import Data.Aeson
import Data.Text
import Distribution.Types.Orphans ()
import Foliage.Meta
import Foliage.Utils.Aeson
import Network.URI (URI)

deriving via MyAesonEncoding PackageMeta instance ToJSON PackageMeta

deriving via MyAesonEncoding PackageMetaEntry instance ToJSON PackageMetaEntry

deriving via MyAesonEncoding RevisionSpec instance ToJSON RevisionSpec

deriving via MyAesonEncoding DeprecationSpec instance ToJSON DeprecationSpec

deriving via MyAesonEncoding PackageVersionSpec instance ToJSON PackageVersionSpec

deriving via Text instance ToJSON GitHubRepo

deriving via Text instance ToJSON GitHubRev

instance ToJSON PackageVersionSource where
  toJSON =
    genericToJSON
      defaultOptions
        { sumEncoding = ObjectWithSingleField,
          omitNothingFields = True
        }

instance ToJSON URI where
  toJSON :: URI -> Value
  toJSON = toJSON . show
