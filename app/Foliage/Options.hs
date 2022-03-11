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

newtype Options = Options
  { optionsConfig :: FilePath
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
