{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Aeson where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.List.NonEmpty qualified as NE
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens hiding ((.=))
import Distribution.Compat.Newtype
import Distribution.Compiler
import Distribution.FieldGrammar
import Distribution.Fields
import Distribution.ModuleName hiding (fromString)
import Distribution.PackageDescription
import Distribution.PackageDescription.FieldGrammar
import Distribution.Pretty
import Distribution.System
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Path
import Distribution.Utils.ShortText qualified as ST
import Language.Haskell.Extension

newtype ViaPretty a = ViaPretty a

instance Pretty a => ToJSON (ViaPretty a) where
  toJSON (ViaPretty a) = toJSON $ prettyShow a

newtype ViaUnpack a = ViaUnpack a

instance (ToJSON o, Newtype o n) => ToJSON (ViaUnpack n) where
  toJSON (ViaUnpack n) = toJSON $ unpack n

deriving via String instance ToJSON Token

deriving via String instance ToJSON Token'

deriving via String instance ToJSON FilePathNT

deriving via String instance ToJSON CompatFilePath

deriving via ViaUnpack CompatLicenseFile instance ToJSON CompatLicenseFile

deriving via (ViaPretty VersionRange) instance ToJSON VersionRange

deriving via ViaUnpack TestedWith instance ToJSON TestedWith

deriving via (ViaPretty CompilerFlavor) instance ToJSON CompilerFlavor

deriving via (ViaPretty SpecVersion) instance ToJSON SpecVersion

deriving via (ViaPretty SpecLicense) instance ToJSON SpecLicense

deriving via (ViaUnpack (List sep b a)) instance ToJSON a => ToJSON (List sep b a)

deriving via (ViaPretty (SymbolicPath from to)) instance ToJSON (SymbolicPath from to)

deriving via (ViaPretty BuildType) instance ToJSON BuildType

deriving via (ViaPretty PackageName) instance ToJSON PackageName

deriving via (ViaPretty Version) instance ToJSON Version

instance ToJSON RepoType

instance ToJSON KnownRepoType

deriving via (ViaPretty Extension) instance ToJSON Extension

deriving via (ViaPretty Language) instance ToJSON Language

deriving via (ViaUnpack (MQuoted a)) instance ToJSON a => ToJSON (MQuoted a)

deriving via (ViaPretty Dependency) instance ToJSON Dependency

deriving via (ViaPretty BenchmarkType) instance ToJSON BenchmarkType

deriving via (ViaPretty ForeignLibType) instance ToJSON ForeignLibType

deriving via (ViaPretty TestType) instance ToJSON TestType

deriving via (ViaPretty ExecutableScope) instance ToJSON ExecutableScope

deriving via (ViaPretty ForeignLibOption) instance ToJSON ForeignLibOption

deriving via (ViaPretty LibVersionInfo) instance ToJSON LibVersionInfo

deriving via (ViaPretty ModuleName) instance ToJSON ModuleName

deriving via (ViaPretty ModuleReexport) instance ToJSON ModuleReexport

deriving via (ViaPretty Mixin) instance ToJSON Mixin

deriving via (ViaPretty PkgconfigDependency) instance ToJSON PkgconfigDependency

deriving via (ViaPretty ExeDependency) instance ToJSON ExeDependency

deriving via (ViaPretty LegacyExeDependency) instance ToJSON LegacyExeDependency

deriving via (ViaPretty LibraryVisibility) instance ToJSON LibraryVisibility

deriving via (ViaPretty FlagName) instance ToJSON FlagName

deriving via (ViaPretty Arch) instance ToJSON Arch

deriving via (ViaPretty OS) instance ToJSON OS

instance ToJSON ConfVar where
  toJSON (OS os) = object ["os" .= os]
  toJSON (Arch arch) = object ["arcg" .= arch]
  toJSON (PackageFlag flag) = object ["os" .= flag]
  toJSON (Impl compiler version)
    | isAnyVersion version = object ["compiler" .= compiler]
    | otherwise = object ["compiler" .= compiler, "version" .= version]

instance ToJSON c => ToJSON (Condition c) where
  toJSON (Var v) = toJSON v
  toJSON (Lit b) = toJSON b
  toJSON (CNot c) = object ["not" .= c]
  toJSON (COr l r) = object ["or" .= [l, r]]
  toJSON (CAnd l r) = object ["and" .= [l, r]]

newtype JSONFieldGrammar s a = JsonFG
  { fieldGrammarJSON :: CabalSpecVersion -> s -> [Pair]
  }
  deriving (Functor)

type JSONFieldGrammar' s = JSONFieldGrammar s s

jsonFieldGrammar :: CabalSpecVersion -> JSONFieldGrammar s a -> s -> [Pair]
jsonFieldGrammar v fg = fieldGrammarJSON fg v

instance Applicative (JSONFieldGrammar s) where
  pure _ = JsonFG (\_ _ -> mempty)
  JsonFG f <*> JsonFG x = JsonFG (\v s -> f v s <> x v s)

instance FieldGrammar ToJSON JSONFieldGrammar where
  blurFieldGrammar :: ALens' a b -> JSONFieldGrammar b d -> JSONFieldGrammar a d
  blurFieldGrammar f (JsonFG fg) = JsonFG $ \v ->
    fg v . aview f

  uniqueFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  uniqueFieldAla fn _pack l = JsonFG $ \_v ->
    jsonField fn . toJSON . pack' _pack . aview l

  booleanFieldDef :: FieldName -> ALens' s Bool -> Bool -> JSONFieldGrammar s Bool
  booleanFieldDef fn l def = JsonFG $ \_v s ->
    let b = aview l s
     in if b == def
          then mempty
          else jsonField fn (toJSON b)

  optionalFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s (Maybe a) -> JSONFieldGrammar s (Maybe a)
  optionalFieldAla fn _pack l = JsonFG $ \_ s ->
    case aview l s of
      Nothing -> mempty
      Just a -> jsonField fn (toJSON (pack' _pack a))

  optionalFieldDefAla :: (ToJSON b, Newtype a b, Eq a) => FieldName -> (a -> b) -> ALens' s a -> a -> JSONFieldGrammar s a
  optionalFieldDefAla fn _pack l def = JsonFG $ \_ s ->
    let x = aview l s
     in if x == def
          then mempty
          else jsonField fn (toJSON (pack' _pack x))

  freeTextField :: FieldName -> ALens' s (Maybe String) -> JSONFieldGrammar s (Maybe String)
  freeTextField fn l = JsonFG $ \_v s ->
    maybe mempty (jsonField fn . toJSON) (aview l s)

  freeTextFieldDef :: FieldName -> ALens' s String -> JSONFieldGrammar s String
  freeTextFieldDef fn l = JsonFG $ \_v ->
    jsonField fn . toJSON . aview l

  freeTextFieldDefST :: FieldName -> ALens' s ST.ShortText -> JSONFieldGrammar s ST.ShortText
  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla :: (ToJSON b, Monoid a, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  monoidalFieldAla fn _pack l = JsonFG $ \_v ->
    jsonField fn . toJSON . pack' _pack . aview l

  prefixedFields :: FieldName -> ALens' s [(String, String)] -> JSONFieldGrammar s [(String, String)]
  prefixedFields fnPfx l = JsonFG $ \_v s ->
    [Key.fromString (fromUTF8BS fnPfx <> n) .= v | (n, v) <- aview l s]

  knownField :: FieldName -> JSONFieldGrammar s ()
  knownField _ = pure ()

  deprecatedSince :: CabalSpecVersion -> String -> JSONFieldGrammar s a -> JSONFieldGrammar s a
  deprecatedSince _ _ x = x

  -- TODO: as PrettyFieldGrammar isn't aware of cabal-version: we output the field
  -- this doesn't affect roundtrip as `removedIn` fields cannot be parsed
  -- so invalid documents can be only manually constructed.
  removedIn _ _ x = x

  availableSince _ _ = id

  hiddenField _ = JsonFG (const mempty)

jsonField :: FieldName -> Value -> [Pair]
jsonField fn v
  | v == emptyArray = mempty
  | v == emptyString = mempty
  | otherwise = [Key.fromString (fromUTF8BS fn) .= v]
  where
    -- Should be added to aeson
    emptyString :: Value
    emptyString = String ""

jsonGenericPackageDescription :: GenericPackageDescription -> Value
jsonGenericPackageDescription gpd =
  object $
    concat
      [ jsonPackageDescription v (packageDescription gpd),
        jsonGenPackageFlags v (genPackageFlags gpd),
        jsonCondLibrary v (condLibrary gpd),
        jsonCondSubLibraries v (condSubLibraries gpd),
        jsonCondForeignLibs v (condForeignLibs gpd),
        jsonCondExecutables v (condExecutables gpd),
        jsonCondTestSuites v (condTestSuites gpd),
        jsonCondBenchmarks v (condBenchmarks gpd)
      ]
  where
    v = specVersion $ packageDescription gpd

jsonPackageDescription :: CabalSpecVersion -> PackageDescription -> [Pair]
jsonPackageDescription v pd@PackageDescription {sourceRepos, setupBuildInfo} =
  jsonFieldGrammar v packageDescriptionFieldGrammar pd
    <> jsonSourceRepos v sourceRepos
    <> jsonSetupBuildInfo v setupBuildInfo

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [Pair]
jsonSourceRepos v =
  concatMap (\neRepos -> ["source-repository" .= NE.map (jsonSourceRepo v) neRepos]) . NE.nonEmpty

jsonSetupBuildInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [Pair]
jsonSetupBuildInfo v =
  concatMap (\sbi -> ["custom-setup" .= jsonFieldGrammar v (setupBInfoFieldGrammar False) sbi])

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> Value
jsonSourceRepo v repo@SourceRepo {repoKind} =
  object $ jsonFieldGrammar v (sourceRepoFieldGrammar repoKind) repo

jsonGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [Pair]
jsonGenPackageFlags v =
  concatMap (\neFlags -> ["flags" .= NE.map (jsonFlag v) neFlags]) . NE.nonEmpty

jsonFlag :: CabalSpecVersion -> PackageFlag -> Value
jsonFlag v flag@(MkPackageFlag name _ _ _) =
  object [Key.fromString (unFlagName name) .= jsonFieldGrammar v (flagFieldGrammar name) flag]

jsonCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [Pair]
jsonCondLibrary v =
  concatMap (\condTree -> ["library" .= object (jsonCondTree v (libraryFieldGrammar LMainLibName) condTree)])

jsonCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [Pair]
jsonCondSubLibraries v =
  concatMap (\neLibs -> ["sub-libraries" .= NE.map (jsonSubLibrary v) neLibs]) . NE.nonEmpty

jsonSubLibrary :: CabalSpecVersion -> (UnqualComponentName, CondTree ConfVar [Dependency] Library) -> Value
jsonSubLibrary v (n, condTree) =
  withName (unUnqualComponentName n) $
    jsonCondTree v (libraryFieldGrammar $ LSubLibName n) condTree

jsonCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [Pair]
jsonCondForeignLibs v =
  concatMap (\neFLibs -> ["foreign-libraries" .= NE.map (jsonForeignLibrary v) neFLibs]) . NE.nonEmpty

jsonForeignLibrary :: CabalSpecVersion -> (UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib) -> Value
jsonForeignLibrary v (n, condTree) =
  withName (unUnqualComponentName n) $
    jsonCondTree v (foreignLibFieldGrammar n) condTree

jsonCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [Pair]
jsonCondExecutables v =
  concatMap (\neExes -> ["executables" .= NE.map (jsonCondExecutable v) neExes]) . NE.nonEmpty

jsonCondExecutable :: CabalSpecVersion -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable) -> Value
jsonCondExecutable v (n, condTree) =
  withName (unUnqualComponentName n) $
    jsonCondTree v (executableFieldGrammar n) condTree

jsonCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [Pair]
jsonCondTestSuites v =
  concatMap (\neSuites -> ["test-suites" .= NE.map (jsonCondTestSuite v) neSuites]) . NE.nonEmpty

jsonCondTestSuite :: CabalSpecVersion -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) -> Value
jsonCondTestSuite v (n, condTree) =
  withName (unUnqualComponentName n) $
    jsonCondTree v testSuiteFieldGrammar (fmap unvalidateTestSuite condTree)

jsonCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [Pair]
jsonCondBenchmarks v =
  concatMap (\neSuites -> ["test-suites" .= NE.map (jsonCondBenchmark v) neSuites]) . NE.nonEmpty

jsonCondBenchmark :: CabalSpecVersion -> (UnqualComponentName, CondTree ConfVar [Dependency] Benchmark) -> Value
jsonCondBenchmark v (n, condTree) =
  withName (unUnqualComponentName n) $
    jsonCondTree v benchmarkFieldGrammar (fmap unvalidateBenchmark condTree)

jsonCondTree :: CabalSpecVersion -> JSONFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [Pair]
jsonCondTree v grammar = goNode
  where
    goNode (CondNode it _ ifs) =
      jsonFieldGrammar v grammar it ++ concatMap goBranch ifs

    goBranch (CondBranch c ifTrue Nothing) =
      [ "if"
          .= object
            [ "cond" .= c,
              "then" .= object (jsonCondTree v grammar ifTrue)
            ]
      ]
    goBranch (CondBranch c ifTrue (Just ifFalse)) =
      [ "if"
          .= object
            [ "cond" .= c,
              "then" .= object (jsonCondTree v grammar ifTrue),
              "else" .= object (jsonCondTree v grammar ifFalse)
            ]
      ]

withName :: ToJSON v => v -> [Pair] -> Value
withName n s = object $ ("name" .= n) : s
