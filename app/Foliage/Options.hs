module Foliage.Options
  ( parseCommand,
    Command (..),
    BuildOptions (..),
    ImportHackageOptions (..),
  )
where

import Foliage.Time
import Options.Applicative

data Command
  = CreateKeys FilePath
  | Build BuildOptions
  | ImportHackage ImportHackageOptions

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
    command "create-keys" (info createKeysCommand (progDesc "Create TUF keys"))
      <> command "build" (info buildCommand (progDesc "Build repository"))
      <> command "import-hackage" (info importHackageCommand (progDesc "Import from Hackage"))

data BuildOptions = BuildOptions
  { buildOptsKeysPath :: FilePath,
    buildOptsCurrentTime :: Maybe UTCTime,
    buildOptsInputDir :: FilePath,
    buildOptsOutputDir :: FilePath
  }

buildCommand :: Parser Command
buildCommand =
  Build
    <$> ( BuildOptions
            <$> strOption
              ( long "keys"
                  <> metavar "KEYS"
                  <> help "TUF keys location"
                  <> showDefault
                  <> value "_keys"
              )
            <*> optional
              ( option
                  (maybeReader iso8601ParseM)
                  ( long "current-time"
                      <> metavar "TIME"
                      <> help "Set current time"
                      <> showDefault
                  )
              )
            <*> strOption
              ( long "input-directory"
                  <> metavar "INPUT"
                  <> help "Repository input directory"
                  <> showDefault
                  <> value "_sources"
              )
            <*> strOption
              ( long "output-directory"
                  <> metavar "OUTPUT"
                  <> help "Repository output directory"
                  <> showDefault
                  <> value "_repo"
              )
        )

createKeysCommand :: Parser Command
createKeysCommand =
  CreateKeys
    <$> strOption
      ( long "keys"
          <> metavar "KEYS"
          <> help "TUF keys location"
          <> showDefault
          <> value "_keys"
      )

newtype ImportHackageOptions = ImportHackageOptions (Maybe String)

importHackageCommand :: Parser Command
importHackageCommand =
  ImportHackage . ImportHackageOptions
    <$> optional
      ( strOption
          ( long "package-name"
              <> metavar "NAME"
              <> help "Filter for package name"
          )
      )
