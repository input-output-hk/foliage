module Foliage.CmdFormat (cmdFormat, formatOptionsParser, FormatOptions) where

import Control.Monad (unless, void, when)
import Data.Algorithm.Diff qualified as Diff
import Data.Algorithm.DiffOutput qualified as Diff
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Development.Shake
import Foliage.Meta (UTCTime, parsePackageVersionSpec, prettyPackageVersionSpec)
import Options.Applicative
import System.FilePath ((</>))

data FormatOptions = FormatOptions
  { formatOptsInputDir :: FilePath,
    formatOptsInPlace :: Bool
  }

formatOptionsParser :: Parser FormatOptions
formatOptionsParser =
  FormatOptions
    <$> strOption
      ( long "input-directory"
          <> metavar "INPUT"
          <> help "Repository input directory"
          <> showDefault
          <> value "_sources"
      )
    <*> switch
      ( long "in-place"
          <> help "Whether to re-write files in-place"
          <> showDefault
      )

cmdFormat :: FormatOptions -> IO ()
cmdFormat formatOptions = do
  shake opts $
    do
      phony "formatAction" (formatAction formatOptions)
      want ["formatAction"]
  where
    cacheDir = "_cache"
    opts = shakeOptions {shakeFiles = cacheDir, shakeVerbosity = Verbose}

data IndexEvent
  = PublicationEvent {timestamp :: UTCTime, meta :: FilePath}
  | MetadataRevisionEvent {timestamp :: UTCTime, meta :: FilePath, revNum :: Int}
  deriving (Eq, Show)

formatAction :: FormatOptions -> Action ()
formatAction
  FormatOptions
    { formatOptsInputDir = inputDir,
      formatOptsInPlace = inplace
    } = do
    metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]

    when (null metaFiles) $ do
      error $
        unlines
          [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)",
            "Make sure you are passing the right input directory. The default input directory is _sources"
          ]

    void $ forP metaFiles $ \metaFile -> do
      let fp = inputDir </> metaFile
      orig <- liftIO $ T.readFile fp
      case parsePackageVersionSpec orig of
        Left err ->
          putError $
            unlines
              [ "Error parsing " <> fp,
                T.unpack err
              ]
        Right parsed -> do
          let reformatted = prettyPackageVersionSpec parsed
              diff = Diff.getGroupedDiff (lines $ T.unpack orig) (lines $ T.unpack reformatted)

          unless (null diff) $
            if inplace
              then do
                putWarn $ "Rewriting " <> fp
                liftIO $ T.writeFile fp reformatted
              else
                putWarn $
                  unlines
                    [ "I would rewrite " <> fp <> " as follows. Pass --in-place to do this automatically.",
                      Diff.ppDiff diff
                    ]
