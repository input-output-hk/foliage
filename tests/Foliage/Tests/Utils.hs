module Foliage.Tests.Utils
  ( checkRequiredProgram,
    callCommand,
    readCommand,
    inTemporaryDirectoryWithFixture,
  )
where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe (isNothing)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkdtemp)
import System.Process (readCreateProcess, shell)

-- | Set up a temporary directory prepopulated with symlinks to the fixture
-- files and change the current directory to it before running the given
-- action. The previous working directory is restored after the action is
-- finished or an exception is raised.
--
-- The first argument should be a relative path from the current directory
-- to the directory containing the fixture files.
inTemporaryDirectoryWithFixture :: FilePath -> IO () -> IO ()
inTemporaryDirectoryWithFixture name action = do
  fixtureDir <- makeAbsolute name
  -- Adding a dot to the prefix to make it look nicer (tests/fixtures/simple123423 vs tests/fixtures/simple.123423)
  let prefix = fixtureDir ++ "."
  withTempDir prefix $ \workDir -> do
    fixtureFiles <- listDirectory fixtureDir
    for_ fixtureFiles $ \p -> createFileLink (fixtureDir </> p) (workDir </> p)
    withCurrentDirectory workDir action

-- | Ensures the given program is available in PATH
checkRequiredProgram :: String -> IO ()
checkRequiredProgram progName =
  findExecutable progName >>= \mpath ->
    when (isNothing mpath) $ fail (progName ++ " is missing")

callCommand :: String -> IO ()
callCommand = void . readCommand

-- | Run a shell command and capture its standard output
readCommand :: String -> IO String
readCommand cmd = readCreateProcess (shell cmd) ""

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tmpDir <- mkdtemp prefix
  action tmpDir `finally` removeDirectoryRecursive tmpDir
