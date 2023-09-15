{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Foliage.Utils.Aeson where

import Data.Aeson
import Data.Coerce
import GHC.Generics

newtype MyAesonEncoding a = MyAesonEncoding a
  deriving (Generic)

myOptions :: Options
myOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , omitNothingFields = True
    }

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (MyAesonEncoding a) where
  toJSON = coerce (genericToJSON myOptions :: a -> Value)
  toEncoding = coerce (genericToEncoding myOptions :: a -> Encoding)
