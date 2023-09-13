{-# LANGUAGE LambdaCase #-}

import Codec.Archive.Tar.Entry (GenEntry (entryContent, entryTime), GenEntryContent (NormalFile))
import Control.Exception (SomeException, try)
import Data.ByteString.Lazy qualified as BL
import Foliage.Tests.Tar
import Foliage.Tests.Utils
import System.Directory
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (EQ)

main :: IO ()
main = do
  checkRequiredProgram "foliage"
  defaultMain $
    testGroup
      "foliage-test-suite"
      [ testCaseSteps "one" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/simple" $ do
            step "Building repository"
            callCommand "foliage build"

            step "Running top-level checks"
            doesDirectoryExist "_keys" @? "_keys does not exist"
            doesFileExist "_repo/01-index.tar" @? "01-index.tar does not exist"
            doesFileExist "_repo/01-index.tar.gz" @? "01-index.tar.gz does not exist"
            doesFileExist "_repo/mirrors.json" @? "mirrors.json does not exist"
            doesFileExist "_repo/root.json" @? "root.json does not exist"
            doesFileExist "_repo/snapshot.json" @? "snapshot.json does not exist"
            doesFileExist "_repo/timestamp.json" @? "timestamp.json does not exist"

            step "Running package checks"
            upstream <- BL.readFile "tarballs/pkg-a/pkg-a.cabal"
            revised <- BL.readFile "_sources/pkg-a/2.3.4.5/revisions/0.cabal"
            revised /= upstream @? "revisions/0.cabal is the same as upstream"
            rev0 <- BL.readFile "_repo/package/pkg-a-2.3.4.5/revision/0.cabal"
            rev0 @?= revised
            built <- BL.readFile "_repo/package/pkg-a-2.3.4.5/pkg-a.cabal"
            built @?= revised

            step "Running latest package checks"
            specificHtml <- BL.readFile "_repo/package/pkg-a-2.3.4.5/index.html"
            latestHtml <- BL.readFile "_repo/package/pkg-a/index.html"
            latestHtml @?= specificHtml

            step "Running tarball checks"
            withTarball "_repo/01-index.tar" $ \TarballAccessFn{lookupEntry} -> do
              lookupEntry "pkg-a/2.3.4.5/pkg-a.cabal" >>= \case
                Nothing ->
                  assertFailure "entry for pkg-a-2.3.4.5 is missing"
                Just entry -> do
                  entryTime entry @?= 1648534790
                  entryContent entry @?= NormalFile revised (BL.length revised)
      , ---
        testCaseSteps "accepts --no-signatures" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/simple" $ do
            step "Building repository"
            callCommand "foliage build --no-signatures"

            step "Running checks"
            not <$> doesDirectoryExist "_keys" @? "_keys exists"
      , ---
        testCaseSteps "accepts --write-metadata" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/simple" $ do
            step "Building repository"
            callCommand "foliage build --write-metadata"

            step "Running checks"
            doesFileExist "_repo/foliage/packages.json" @? "foliage/packages.json does not exist"
      , ---
        testCaseSteps "rejects quoted timestamp in meta.toml" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/bad-timestamp" $ do
            step "Building repository (expecting failure)"
            result <- try @SomeException (callCommand "foliage build --no-signatures")
            case result of
              Left _ -> pure ()
              Right _ -> assertFailure "foliage build should have failed on a quoted timestamp"
      ]
