module Foliage.Tests.Tar where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (Entry (..))
import Codec.Archive.Tar.Index (TarIndexEntry (..))
import Codec.Archive.Tar.Index qualified as Tar
import Control.Monad (when)
import Data.ByteString.Lazy qualified as B
import System.IO
import Test.Tasty.HUnit

newtype TarballAccessFn = TarballAccessFn
  { lookupEntry :: FilePath -> IO (Maybe Entry)
  }

withTarball :: (HasCallStack) => FilePath -> (TarballAccessFn -> IO r) -> IO r
withTarball path action = do
  eIdx <- Tar.build . Tar.read <$> B.readFile path
  case eIdx of
    Left err ->
      assertFailure (show err)
    Right idx ->
      withFile path ReadMode $ \handle -> do
        hIsClosed handle >>= flip when (putStrLn "[after reading] it's closed!")
        let lookupEntry :: FilePath -> IO (Maybe Entry)
            lookupEntry filepath =
              case Tar.lookup idx filepath of
                Just (TarFileEntry offset) -> do
                  Just <$> Tar.hReadEntry handle offset
                _otherwise ->
                  return Nothing
        action $ TarballAccessFn{lookupEntry}
