module Foliage.Tests.FetchURL (fetchURLTests) where

import Data.ByteString qualified as BS
import Foliage.FetchURL (validateGzipFile)
import System.IO (hClose, openTempFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

fetchURLTests :: TestTree
fetchURLTests =
  testGroup
    "validateGzipFile"
    [ testCase "accepts valid gzip file" $ do
        -- Use the actual fixture tarball which is a valid gzip
        result <- validateGzipFile "tests/fixtures/simple/tarballs/pkg-a-2.3.4.5.tar.gz"
        result @?= True
    , testCase "rejects file with invalid magic bytes" $
        withSystemTempDirectory "foliage-test" $ \tmpDir -> do
          (path, h) <- openTempFile tmpDir "invalid.gz"
          -- Write wrong magic bytes (should be 0x1f 0x8b for gzip)
          BS.hPut h (BS.pack [0x00, 0x00, 0x01, 0x02])
          hClose h
          result <- validateGzipFile path
          result @?= False
    , testCase "rejects file with only one byte" $
        withSystemTempDirectory "foliage-test" $ \tmpDir -> do
          (path, h) <- openTempFile tmpDir "tooshort.gz"
          BS.hPut h (BS.pack [0x1f]) -- Only first byte of magic
          hClose h
          result <- validateGzipFile path
          result @?= False
    , testCase "rejects empty file" $
        withSystemTempDirectory "foliage-test" $ \tmpDir -> do
          (path, h) <- openTempFile tmpDir "empty.gz"
          hClose h
          result <- validateGzipFile path
          result @?= False
    , testCase "rejects non-existent file" $ do
        result <- validateGzipFile "/this/path/does/not/exist/file.gz"
        result @?= False
    , testCase "accepts file with correct magic bytes followed by garbage" $
        withSystemTempDirectory "foliage-test" $ \tmpDir -> do
          (path, h) <- openTempFile tmpDir "partial.gz"
          -- Correct magic bytes but invalid rest (we only check magic bytes)
          BS.hPut h (BS.pack [0x1f, 0x8b, 0xFF, 0xFF])
          hClose h
          result <- validateGzipFile path
          result @?= True -- Should pass since we only check first 2 bytes
    ]
