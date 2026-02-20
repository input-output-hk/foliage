module Foliage.CmdRewriteVersion (cmdRewriteVersion) where

import Data.Maybe (fromMaybe)
import Distribution.Compat.Lens (Lens', (&), (.~), (^.))
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Lens (GenericPackageDescription, package, packageDescription, pkgVersion)
import Distribution.Types.Version (Version)
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (normal)

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
