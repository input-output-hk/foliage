{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Time
  ( iso8601ParseM,
    iso8601Show,
    truncateSeconds,
    module Data.Time,
    module Data.Time.LocalTime,
    module Data.Time.Clock.POSIX,
  )
where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Compat ()
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Development.Shake.Classes

instance Binary UTCTime where
  get = iso8601ParseM =<< get
  put = put . iso8601Show

truncateSeconds :: UTCTime -> UTCTime
truncateSeconds = posixSecondsToUTCTime . fromIntegral @Int . floor . utcTimeToPOSIXSeconds
