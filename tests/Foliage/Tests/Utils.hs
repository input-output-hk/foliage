module Foliage.Tests.Utils
  ( checkRequiredProgram,
    callCommand,
    readCommand,
    withFixture,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe (isNothing)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkdtemp)
import System.Process (readCreateProcess, shell)

-- | Set up a temporary directory prepopulated with symlinks to the fixture files. The first argument
-- should be a relative path from the current directory to the directory containing the
-- fixture files.
withFixture :: FilePath -> IO () -> IO ()
withFixture name =
  bracket acquire release . const
  where
    acquire = do
      cur <- getCurrentDirectory
      fixtureDir <- makeAbsolute name
      -- Adding a dot to the prefix to make it look nicer (tests/fixtures/simple123423 vs tests/fixtures/simple.123423)
      workDir <- mkdtemp $ fixtureDir ++ "."
      setCurrentDirectory workDir
      fixtureFiles <- listDirectory fixtureDir
      for_ fixtureFiles $ \p -> createFileLink (fixtureDir </> p) (workDir </> p)
      return (cur, workDir)

    release (old, workDir) = do
      setCurrentDirectory old
      removeDirectoryRecursive workDir

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
