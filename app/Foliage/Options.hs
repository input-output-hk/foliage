{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Foliage.Options
  ( parseCommand,
    Command (..),
    BuildOptions (..),
    SignOptions (..),
    ImportIndexOptions (..),
    ImportFilter (..),
  )
where

import Development.Shake.Classes (Binary, Hashable, NFData)
import Foliage.Time
import GHC.Generics
import Options.Applicative

data Command
  = CreateKeys FilePath
  | Build BuildOptions
  | ImportIndex ImportIndexOptions

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
      <> command "import-index" (info importIndexCommand (progDesc "Import from Hackage index"))

data SignOptions
  = SignOptsSignWithKeys FilePath
  | SignOptsDon'tSign
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Hashable, NFData)

data BuildOptions = BuildOptions
  { buildOptsSignOpts :: SignOptions,
    buildOptsCurrentTime :: Maybe UTCTime,
    buildOptsExpireSignaturesOn :: Maybe UTCTime,
    buildOptsInputDir :: FilePath,
    buildOptsOutputDir :: FilePath,
    buildOptsNumThreads :: Int,
    buildOptsWriteMetadata :: Bool
  }

buildCommand :: Parser Command
buildCommand =
  Build
    <$> ( BuildOptions
            <$> signOpts
            <*> optional
              ( option
                  (maybeReader iso8601ParseM)
                  ( long "current-time"
                      <> metavar "TIME"
                      <> help "Set current time"
                      <> showDefault
                  )
              )
            <*> optional
              ( option
                  (maybeReader iso8601ParseM)
                  ( long "expire-signatures-on"
                      <> metavar "TIME"
                      <> help "Set an expiry date on TUF signatures"
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
            <*> option
              auto
              ( long "num-jobs"
                  <> short 'j'
                  <> metavar "JOBS"
                  <> help "Number of jobs to run in parallel, 0 is 'all available cores'"
                  <> showDefault
                  <> value 1
              )
            <*> switch
              ( long "write-metadata"
                  <> help "Write metadata in the output-directory"
                  <> showDefault
              )
        )
  where
    signOpts =
      ( SignOptsSignWithKeys
          <$> strOption
            ( long "keys"
                <> metavar "KEYS"
                <> help "TUF keys location"
                <> showDefault
                <> value "_keys"
            )
      )
        <|> ( SignOptsDon'tSign
                <$ switch (long "no-signatures" <> help "Don't sign the repository")
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

data ImportFilter = ImportFilter String (Maybe String)

newtype ImportIndexOptions = ImportIndexOptions
  { importOptsFilter :: Maybe ImportFilter
  }

importIndexCommand :: Parser Command
importIndexCommand =
  ImportIndex . ImportIndexOptions
    <$> optional
      ( ImportFilter
          <$> strOption
            ( long "package-name"
                <> metavar "NAME"
                <> help "package name"
            )
          <*> optional
            ( strOption
                ( long "package-version"
                    <> metavar "VERSION"
                    <> help "package version"
                )
            )
      )
