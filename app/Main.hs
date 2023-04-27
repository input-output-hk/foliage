{-# LANGUAGE LambdaCase #-}

module Main where

import Foliage.CmdBuild (BuildOptions, buildOptionsParser, cmdBuild)
import Foliage.CmdCreateKeys (CreateKeysOptions, cmdCreateKeys, createKeysOptionsParser)
import Foliage.CmdFormat (FormatOptions, cmdFormat, formatOptionsParser)
import Foliage.CmdImportIndex (ImportIndexOptions, cmdImportIndex, importIndexOptionsParser)
import Main.Utf8 (withUtf8)
import Options.Applicative

data Command
  = CreateKeys CreateKeysOptions
  | Build BuildOptions
  | ImportIndex ImportIndexOptions
  | Format FormatOptions

parseCommand :: IO Command
parseCommand =
  customExecParser
    (prefs showHelpOnEmpty)
    $ info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "foliage"
          <> header "foliage - a builder for static Hackage repositories"
      )

optionsParser :: Parser Command
optionsParser =
  hsubparser $
    mconcat
      [ command "create-keys" (info (CreateKeys <$> createKeysOptionsParser) (progDesc "Create TUF keys")),
        command "build" (info (Build <$> buildOptionsParser) (progDesc "Build repository")),
        command "import-index" (info (ImportIndex <$> importIndexOptionsParser) (progDesc "Import from Hackage index")),
        command "format" (info (Format <$> formatOptionsParser) (progDesc "Format metadata files"))
      ]

main :: IO ()
main = withUtf8 $ do
  putStrLn "🌿 Foliage"
  parseCommand >>= \case
    CreateKeys path -> cmdCreateKeys path
    Build buildOpts -> cmdBuild buildOpts
    ImportIndex importIndexOpts -> cmdImportIndex importIndexOpts
    Format formatOpts -> cmdFormat formatOpts
