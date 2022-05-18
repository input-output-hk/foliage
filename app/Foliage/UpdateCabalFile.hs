module Foliage.UpdateCabalFile (rewritePackageVersion) where

import Distribution.PackageDescription.Parsec as Cabal
import Distribution.PackageDescription.PrettyPrint as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Types.PackageDescription as Cabal
import Distribution.Types.PackageId as Cabal
import Distribution.Types.Version
import Distribution.Verbosity as Cabal

rewritePackageVersion ::
  -- | path to @.cabal@ file
  FilePath ->
  -- | new version
  Version ->
  IO ()
rewritePackageVersion cabalPath ver = do
  gpd <- Cabal.readGenericPackageDescription Cabal.normal cabalPath
  let setVersion ::
        Version ->
        Cabal.PackageDescription ->
        Cabal.PackageDescription
      setVersion v' pd =
        pd {Cabal.package = (Cabal.package pd) {Cabal.pkgVersion = v'}}

      pd = Cabal.packageDescription gpd
      pd' = setVersion ver pd

  Cabal.writePackageDescription cabalPath pd'
