module Foliage.CmdLint (cmdLint, lintOptionsParser, LintOptions) where

import Control.Monad (void, when)
import Development.Shake
import Foliage.Meta (readPackageVersionSpec, writePackageVersionSpec)
import Options.Applicative
import System.FilePath ((</>))

newtype LintOptions = LintOptions {lintOptsInputDir :: FilePath}

lintOptionsParser :: Parser LintOptions
lintOptionsParser =
  LintOptions
    <$> strOption
      ( long "input-directory"
          <> metavar "INPUT"
          <> help "Repository input directory"
          <> showDefault
          <> value "_sources"
      )

cmdLint :: LintOptions -> IO ()
cmdLint buildOptions = do
  shake opts $
    do
      phony "lintAction" (lintAction buildOptions)
      want ["lintAction"]
  where
    cacheDir = "_cache"
    opts = shakeOptions {shakeFiles = cacheDir, shakeVerbosity = Verbose}

lintAction :: LintOptions -> Action ()
lintAction LintOptions {lintOptsInputDir = inputDir} = do
  metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

  when (null metaFiles) $ do
    error $
      unlines
        [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)",
          "Make sure you are passing the right input directory. The default input directory is _sources"
        ]

  void $ forP metaFiles $ \metaFile ->
    traced ("rewriting " <> metaFile) $
      let fp = inputDir </> metaFile
       in readPackageVersionSpec fp >>= writePackageVersionSpec fp
