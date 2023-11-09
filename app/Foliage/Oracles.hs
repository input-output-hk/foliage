{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Foliage.Oracles where

import Data.Foldable (for_)
import Data.Traversable (for)
import Development.Shake
import Development.Shake.Classes
import Foliage.HackageSecurity (Key, Some, readJSONSimple)
import Foliage.Options (SignOptions (..))
import Foliage.Time qualified as Time
import GHC.Generics (Generic)
import Hackage.Security.Client (
  RepoLayout,
  RepoPath,
  anchorRepoPathLocally,
  hackageRepoLayout,
 )
import Hackage.Security.Util.Path qualified as Sec
import Hackage.Security.Util.Pretty qualified as Sec
import System.FilePath ((</>))

-- | Just a shortcut to write types
type Oracle q = q -> Action (RuleResult q)

data InputRoot

type InputPath = Sec.Path InputRoot

instance Sec.Pretty InputPath where
  pretty (Sec.Path fp) = "<input>/" ++ fp

data InputDir = InputDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult InputDir = FilePath

addInputDirOracle :: FilePath -> Rules (Oracle InputDir)
addInputDirOracle inputDir =
  addOracle $ \InputDir -> return inputDir

anchorInputPath :: InputPath -> Action (Sec.Path Sec.Absolute)
anchorInputPath ip = do
  inputDir <- askOracle InputDir
  inputDirRoot <- liftIO $ Sec.makeAbsolute (Sec.fromFilePath inputDir)
  return $ inputDirRoot Sec.</> Sec.unrootPath ip

data CacheRoot

type CachePath = Sec.Path CacheRoot

instance Sec.Pretty CachePath where
  pretty (Sec.Path fp) = "<cache>/" ++ fp

data CacheDir = CacheDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult CacheDir = FilePath

addCacheDirOracle :: FilePath -> Rules (Oracle CacheDir)
addCacheDirOracle inputDir =
  addOracle $ \CacheDir -> return inputDir

anchorCachePath :: CachePath -> Action (Sec.Path Sec.Absolute)
anchorCachePath ip = do
  cacheDir <- askOracle CacheDir
  cacheDirRoot <- liftIO $ Sec.makeAbsolute (Sec.fromFilePath cacheDir)
  return $ cacheDirRoot Sec.</> Sec.unrootPath ip

data OutputDir = OutputDir
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OutputDir = FilePath

getOutputDir :: Action (Sec.Path Sec.Absolute)
getOutputDir = do
  outputDirRoot <- askOracle OutputDir
  liftIO $ Sec.makeAbsolute (Sec.fromFilePath outputDirRoot)

anchorRepoPath :: RepoPath -> Action (Sec.Path Sec.Absolute)
anchorRepoPath rp = do
  outputDir <- getOutputDir
  return $ anchorRepoPathLocally outputDir rp

anchorRepoPath' :: (RepoLayout -> RepoPath) -> Action (Sec.Path Sec.Absolute)
anchorRepoPath' p = do
  outputDir <- getOutputDir
  return $ anchorRepoPathLocally outputDir $ p hackageRepoLayout

addOutputDirOracle :: FilePath -> Rules (Oracle OutputDir)
addOutputDirOracle outputDir =
  addOracle $ \OutputDir -> return outputDir

data CurrentTime = CurrentTime
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

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
  addOracle $ \CurrentTime -> return currentTime

data ExpiryTime = ExpiryTime
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult ExpiryTime = Maybe Time.UTCTime

addExpiryTimeOracle :: Maybe Time.UTCTime -> Rules (Oracle ExpiryTime)
addExpiryTimeOracle mExpireSignaturesOn = do
  liftIO $ for_ mExpireSignaturesOn $ \expireSignaturesOn ->
    putStrLn $ "Expiry time set to " <> Time.iso8601Show expireSignaturesOn
  addOracle $ \ExpiryTime -> return mExpireSignaturesOn

data SignOptionsOracle = SignOptionsOracle
  deriving stock (Show, Generic, Typeable, Eq)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult SignOptionsOracle = SignOptions

addSigninigKeysOracle :: SignOptions -> Rules (Oracle SignOptionsOracle)
addSigninigKeysOracle signOpts =
  addOracle $ \SignOptionsOracle -> return signOpts

readKeys :: FilePath -> Action [Some Key]
readKeys base = do
  askOracle SignOptionsOracle >>= \case
    SignOptsSignWithKeys keysPath -> do
      paths <- getDirectoryFiles (keysPath </> base) ["*.json"]
      need $ map (\fn -> keysPath </> base </> fn) paths
      for paths $ \path -> do
        mKey <- liftIO $ readJSONSimple (keysPath </> base </> path)
        case mKey of
          Left err -> fail $ show err
          Right key -> pure key
    SignOptsDon'tSign ->
      return []
