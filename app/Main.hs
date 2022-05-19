module Main where

import Foliage.CmdBuild
import Foliage.CmdCreateKeys
import Foliage.CmdImportIndex
import Foliage.Options

main :: IO ()
main = do
  putStrLn "🌿 Foliage"
  parseCommand >>= \case
    CreateKeys path -> cmdCreateKeys path
    Build buildOpts -> cmdBuild buildOpts
    ImportIndex importIndexOpts -> cmdImportIndex importIndexOpts
