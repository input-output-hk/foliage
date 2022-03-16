module Foliage.Options
  ( parseOptions,
    Options (..),
    module Options.Applicative,
  )
where

import Options.Applicative

parseOptions :: IO Options
parseOptions =
  execParser $
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "foliage"
          <> header "foliage - a builder for static Hackage repositories"
      )

data Options = Options
  { optionsConfig :: FilePath
  , optionsKeys :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> help "Config file"
          <> showDefault
          <> value "config.toml"
      )
    <*> strOption
      ( long "keys"
          <> metavar "KEYS"
          <> help "Keys folder"
          <> showDefault
          <> value "_keys"
      )

