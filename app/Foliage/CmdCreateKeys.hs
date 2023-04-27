module Foliage.CmdCreateKeys
  ( cmdCreateKeys,
    createKeysOptionsParser,
    CreateKeysOptions (..),
  )
where

import Foliage.HackageSecurity
import Options.Applicative

newtype CreateKeysOptions = CreateKeysOptions {createKeysOptsPath :: FilePath}

createKeysOptionsParser :: Parser CreateKeysOptions
createKeysOptionsParser =
  CreateKeysOptions
    <$> strOption
      ( long "keys"
          <> metavar "KEYS"
          <> help "TUF keys location"
          <> showDefault
          <> value "_keys"
      )

cmdCreateKeys :: CreateKeysOptions -> IO ()
cmdCreateKeys CreateKeysOptions {createKeysOptsPath = keyPath} = do
  putStrLn $ "Creating a new set of keys in " <> keyPath
  createKeys keyPath
