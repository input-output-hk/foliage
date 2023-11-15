{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Foliage.Time (
  truncateSeconds,
)
where

import Data.Time (UTCTime (..))
import Data.Time.Clock.POSIX (
  posixSecondsToUTCTime,
  utcTimeToPOSIXSeconds,
 )
import Data.Time.Compat ()
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Development.Shake.Classes (Binary (..))

instance Binary UTCTime where
  get = iso8601ParseM =<< get
  put = put . iso8601Show

truncateSeconds :: UTCTime -> UTCTime
truncateSeconds = posixSecondsToUTCTime . fromIntegral @Int . floor . utcTimeToPOSIXSeconds
