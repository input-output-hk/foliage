{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- This module is a partial copy of `Distribution.PackageDescription.FieldGrammar`.
--
-- It is needed because that module does not export CompatFilePath and
-- CompatLicenseFile, which are needed to call packageDescriptionFieldGrammar
--
module Distribution.PackageDescription.FieldGrammar.Copy
  ( module Distribution.PackageDescription.FieldGrammar,
    packageDescriptionFieldGrammar,
    CompatFilePath (..),
    CompatLicenseFile (..),
  )
where

import Distribution.CabalSpecVersion
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Newtype (Newtype, pack', unpack')
import Distribution.Compat.Prelude
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.FieldGrammar hiding (packageDescriptionFieldGrammar)
import Distribution.Parsec
import Distribution.Pretty (Pretty (..), showToken)
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Types.Lens as L
import Distribution.Utils.Path
import Distribution.Version (Version, VersionRange)
import Prelude ()

packageDescriptionFieldGrammar ::
  ( FieldGrammar c g,
    Applicative (g PackageDescription),
    Applicative (g PackageIdentifier),
    c (Identity BuildType),
    c (Identity PackageName),
    c (Identity Version),
    c (List FSep FilePathNT String),
    c (List FSep CompatFilePath String),
    c (List FSep (Identity (SymbolicPath PackageDir LicenseFile)) (SymbolicPath PackageDir LicenseFile)),
    c (List FSep TestedWith (CompilerFlavor, VersionRange)),
    c (List VCat FilePathNT String),
    c FilePathNT,
    c CompatLicenseFile,
    c CompatFilePath,
    c SpecLicense,
    c SpecVersion
  ) =>
  g PackageDescription PackageDescription
packageDescriptionFieldGrammar =
  PackageDescription
    <$> optionalFieldDefAla "cabal-version" SpecVersion L.specVersion CabalSpecV1_0
    <*> blurFieldGrammar L.package packageIdentifierGrammar
    <*> optionalFieldDefAla "license" SpecLicense L.licenseRaw (Left SPDX.NONE)
    <*> licenseFilesGrammar
    <*> freeTextFieldDefST "copyright" L.copyright
    <*> freeTextFieldDefST "maintainer" L.maintainer
    <*> freeTextFieldDefST "author" L.author
    <*> freeTextFieldDefST "stability" L.stability
    <*> monoidalFieldAla "tested-with" (alaList' FSep TestedWith) L.testedWith
    <*> freeTextFieldDefST "homepage" L.homepage
    <*> freeTextFieldDefST "package-url" L.pkgUrl
    <*> freeTextFieldDefST "bug-reports" L.bugReports
    <*> pure [] -- source-repos are stanza
    <*> freeTextFieldDefST "synopsis" L.synopsis
    <*> freeTextFieldDefST "description" L.description
    <*> freeTextFieldDefST "category" L.category
    <*> prefixedFields "x-" L.customFieldsPD
    <*> optionalField "build-type" L.buildTypeRaw
    <*> pure Nothing -- custom-setup
    -- components
    <*> pure Nothing -- lib
    <*> pure [] -- sub libs
    <*> pure [] -- executables
    <*> pure [] -- foreign libs
    <*> pure [] -- test suites
    <*> pure [] -- benchmarks
    --  * Files
    <*> monoidalFieldAla "data-files" (alaList' VCat FilePathNT) L.dataFiles
    <*> optionalFieldDefAla "data-dir" CompatFilePath L.dataDir "."
    ^^^ fmap (\x -> if null x then "." else x) -- map empty directories to "."
    <*> monoidalFieldAla "extra-source-files" formatExtraSourceFiles L.extraSrcFiles
    <*> monoidalFieldAla "extra-tmp-files" (alaList' VCat FilePathNT) L.extraTmpFiles
    <*> monoidalFieldAla "extra-doc-files" (alaList' VCat FilePathNT) L.extraDocFiles
  where
    packageIdentifierGrammar =
      PackageIdentifier
        <$> uniqueField "name" L.pkgName
        <*> uniqueField "version" L.pkgVersion

    licenseFilesGrammar =
      (++)
        -- TODO: neither field is deprecated
        -- should we pretty print license-file if there's single license file
        -- and license-files when more
        <$> monoidalFieldAla "license-file" CompatLicenseFile L.licenseFiles
        <*> monoidalFieldAla "license-files" (alaList FSep) L.licenseFiles
        ^^^ hiddenField

-------------------------------------------------------------------------------
-- newtypes
-------------------------------------------------------------------------------

-- | Compat FilePath accepts empty file path,
-- but issues a warning.
--
-- There are simply too many (~1200) package definition files
--
-- @
-- license-file: ""
-- @
--
-- and
--
-- @
-- data-dir: ""
-- @
--
-- across Hackage to outrule them completely.
-- I suspect some of them are generated (e.g. formatted) by machine.
newtype CompatFilePath = CompatFilePath {getCompatFilePath :: FilePath} -- TODO: Change to use SymPath

instance Newtype String CompatFilePath

instance Parsec CompatFilePath where
  parsec = do
    token <- parsecToken
    if null token
      then do
        parsecWarning PWTEmptyFilePath "empty FilePath"
        return (CompatFilePath "")
      else return (CompatFilePath token)

instance Pretty CompatFilePath where
  pretty = showToken . getCompatFilePath

newtype CompatLicenseFile = CompatLicenseFile {getCompatLicenseFile :: [SymbolicPath PackageDir LicenseFile]}

instance Newtype [SymbolicPath PackageDir LicenseFile] CompatLicenseFile

-- TODO
instance Parsec CompatLicenseFile where
  parsec = emptyToken <|> CompatLicenseFile . unpack' (alaList FSep) <$> parsec
    where
      emptyToken = P.try $ do
        token <- parsecToken
        if null token
          then return (CompatLicenseFile [])
          else P.unexpected "non-empty-token"

instance Pretty CompatLicenseFile where
  pretty = pretty . pack' (alaList FSep) . getCompatLicenseFile
