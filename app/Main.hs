module Main where

import Foliage.CmdBuild
import Foliage.CmdCreateKeys
import Foliage.CmdImportHackage
import Foliage.Options

main :: IO ()
main = do
  putStrLn "🌿 Foliage"
  parseCommand >>= \case
    CreateKeys path -> cmdCreateKeys path
    Build buildOpts -> cmdBuild buildOpts
    ImportHackage importHackageOpts -> cmdImportHackage importHackageOpts
