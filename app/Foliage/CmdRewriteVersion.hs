module Foliage.CmdRewriteVersion (cmdRewriteVersion) where

import Data.Maybe (fromMaybe)
import Distribution.Compat.Lens (Lens', (&), (.~), (^.))
import Distribution.PackageDescription.PrettyPrint
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription
import Distribution.Types.Lens
import Distribution.Types.Version (Version)
import Distribution.Utils.Path
import Distribution.Verbosity

cmdRewriteVersion
  :: String
  -- ^ new version
  -> FilePath
  -- ^ path to @.cabal@ file
  -> IO ()
cmdRewriteVersion versionStr cabalPath = do
  let
    version = fromMaybe (error $ "invalid package version: " <> versionStr) $ simpleParsec versionStr
    gpdVersion :: Lens' GenericPackageDescription Version
    gpdVersion = packageDescription . package . pkgVersion
  gpd <- readGenericPackageDescription normal Nothing (makeSymbolicPath cabalPath)
  putStrLn . unwords $
    [ "Rewriting the version from"
    , prettyShow (gpd ^. gpdVersion)
    , "to"
    , prettyShow version
    , "in"
    , cabalPath
    ]
  writeGenericPackageDescription cabalPath $
    gpd & gpdVersion .~ version
