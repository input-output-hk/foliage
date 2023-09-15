module Foliage.UpdateCabalFile (rewritePackageVersion) where

import Distribution.Compat.Lens (set)
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple.PackageDescription
import Distribution.Types.Lens
import Distribution.Types.Version
import Distribution.Verbosity

rewritePackageVersion
  :: FilePath
  -- ^ path to @.cabal@ file
  -> Version
  -- ^ new version
  -> IO ()
rewritePackageVersion cabalPath ver = do
  gpd <- readGenericPackageDescription normal cabalPath
  writeGenericPackageDescription cabalPath (set (packageDescription . package . pkgVersion) ver gpd)
