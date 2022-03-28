{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Time
  ( iso8601ParseM,
    iso8601Show,
    getCurrentTime,
    UTCTime (..),
    utcTimeToPOSIXSeconds,
    addUTCTime,
    nominalDay,
    truncateSeconds,
  )
where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Compat ()
import Data.Time.Format.ISO8601
import Development.Shake.Classes

instance Binary UTCTime where
  get = iso8601ParseM =<< get
  put = put . iso8601Show

truncateSeconds :: UTCTime -> UTCTime
truncateSeconds = posixSecondsToUTCTime . fromIntegral @Int . floor . utcTimeToPOSIXSeconds
