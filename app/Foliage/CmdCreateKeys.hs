module Foliage.CmdCreateKeys where
import Foliage.HackageSecurity

cmdCreateKeys :: FilePath -> IO ()
cmdCreateKeys keyPath = do
  putStrLn $ "Creating a new set of keys in " <> keyPath
  createKeys keyPath

