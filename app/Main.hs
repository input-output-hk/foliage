module Main where

import Foliage.CmdBuild
import Foliage.CmdCreateKeys
import Foliage.CmdImportIndex
import Foliage.Options
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ do
  putStrLn "🌿 Foliage"
  parseCommand >>= \case
    CreateKeys path -> cmdCreateKeys path
    Build buildOpts -> cmdBuild buildOpts
    ImportIndex importIndexOpts -> cmdImportIndex importIndexOpts
