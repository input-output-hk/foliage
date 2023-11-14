{-# LANGUAGE TypeFamilies #-}

module Foliage.Oracles where

import Development.Shake
import Development.Shake.Classes
import Foliage.Time qualified as Time
import GHC.Generics (Generic)

-- FIXME: consider using configuration variables (usingConfig/getConfig) or shakeExtra

-- | Just a shortcut to write types
type Oracle q = q -> Action (RuleResult q)

newtype CacheDir = CacheDir ()
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

type instance RuleResult CacheDir = FilePath

addCacheDirOracle :: FilePath -> Rules (Oracle CacheDir)
addCacheDirOracle inputDir =
  addOracle $ \CacheDir{} -> return inputDir

newtype CurrentTime = CurrentTime ()
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

type instance RuleResult CurrentTime = Time.UTCTime

addCurrentTimeOracle :: Maybe Time.UTCTime -> Rules (Oracle CurrentTime)
addCurrentTimeOracle mCurrentTime = do
  currentTime <- case mCurrentTime of
    Nothing -> do
      t <- Time.truncateSeconds <$> liftIO Time.getCurrentTime
      liftIO $ putStrLn $ "Current time set to " <> Time.iso8601Show t <> ". You can set a fixed time using the --current-time option."
      return t
    Just t -> do
      liftIO $ putStrLn $ "Current time set to " <> Time.iso8601Show t <> "."
      return t
  addOracle $ \CurrentTime{} -> return currentTime
