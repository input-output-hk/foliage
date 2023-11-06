{-# LANGUAGE LambdaCase #-}

import Codec.Archive.Tar.Entry (Entry (..))
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

            step "Running checks"
            doesDirectoryExist "_keys" @? "_keys does not exist"
            doesFileExist "_repo/01-index.tar" @? "01-index.tar does not exist"
            doesFileExist "_repo/01-index.tar.gz" @? "01-index.tar.gz does not exist"
            doesFileExist "_repo/mirrors.json" @? "mirrors.json does not exist"
            doesFileExist "_repo/root.json" @? "root.json does not exist"
            doesFileExist "_repo/snapshot.json" @? "snapshot.json does not exist"
            doesFileExist "_repo/timestamp.json" @? "timestamp.json does not exist"

            withTarball "_repo/01-index.tar" $ \TarballAccessFn{lookupEntry} -> do
              lookupEntry "pkg-a/2.3.4.5/pkg-a.cabal" >>= \case
                Nothing ->
                  assertFailure "entry for pkg-a-2.3.4.5 is missing"
                Just entry -> do
                  entryTime entry @?= 1648534790
      , ---
        testCaseSteps "git submodules" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/git-submodule" $ do
            step "Building repository"
            callCommand "foliage build"

            doesFileExist "_cache/git/cardano-scaling/foliage-test-with-submodule/README.md" @? "Missing working copy"
            doesFileExist "_cache/foliage-test-with-submodule/1.0.0/README.md" @? "Missing packaged version"
            doesFileExist "_cache/foliage-test-with-submodule/1.1.0/README.md" @? "Missing packaged version"
      , ---
        testCaseSteps "accepts --no-signatures" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/simple" $ do
            step "Building repository"
            callCommand "foliage build --no-signatures"

            step "Running checks"
            doesExist <- doesDirectoryExist "_keys"
            doesExist @?= False
      , ---
        testCaseSteps "accepts --write-metadata" $ \step ->
          inTemporaryDirectoryWithFixture "tests/fixtures/simple" $ do
            step "Building repository"
            callCommand "foliage build --write-metadata"

            step "Running checks"
            doesFileExist "_repo/foliage/packages.json" @? "foliage/packages.json does not exist"
      ]
