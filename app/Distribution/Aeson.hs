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
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Bifunctor (second)
import Data.List (foldl1')
import Data.String (fromString)
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
  toJSON (Impl compiler version) = object ["compiler" .= compiler, "version" .= version]

instance ToJSON c => ToJSON (Condition c) where
  toJSON (Var v) = toJSON v
  toJSON (Lit b) = toJSON b
  toJSON (CNot c) = object ["not" .= c]
  toJSON (COr l r) = object ["or" .= [l, r]]
  toJSON (CAnd l r) = object ["and" .= [l, r]]

newtype JSONFieldGrammar s a = JsonFG
  { fieldGrammarJSON :: CabalSpecVersion -> [Condition ConfVar] -> s -> [Pair]
  }
  deriving (Functor)

type JSONFieldGrammar' s = JSONFieldGrammar s s

jsonFieldGrammar :: CabalSpecVersion -> [Condition ConfVar] -> JSONFieldGrammar s a -> s -> [Pair]
jsonFieldGrammar v cs fg = fieldGrammarJSON fg v cs

instance Applicative (JSONFieldGrammar s) where
  pure _ = JsonFG (\_ _ _ -> mempty)
  JsonFG f <*> JsonFG x = JsonFG (\v cs s -> f v cs s <> x v cs s)

instance FieldGrammar ToJSON JSONFieldGrammar where
  blurFieldGrammar :: ALens' a b -> JSONFieldGrammar b d -> JSONFieldGrammar a d
  blurFieldGrammar f (JsonFG fg) = JsonFG $ \v cs ->
    fg v cs . aview f

  uniqueFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  uniqueFieldAla fn _pack l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . pack' _pack . aview l

  booleanFieldDef :: FieldName -> ALens' s Bool -> Bool -> JSONFieldGrammar s Bool
  booleanFieldDef fn l def = JsonFG $ \_v cs s ->
    let b = aview l s
     in if b == def
          then mempty
          else jsonField cs fn (toJSON b)

  optionalFieldAla :: (ToJSON b, Newtype a b) => FieldName -> (a -> b) -> ALens' s (Maybe a) -> JSONFieldGrammar s (Maybe a)
  optionalFieldAla fn _pack l = JsonFG $ \_ cs s ->
    case aview l s of
      Nothing -> mempty
      Just a -> jsonField cs fn (toJSON (pack' _pack a))

  optionalFieldDefAla :: (ToJSON b, Newtype a b, Eq a) => FieldName -> (a -> b) -> ALens' s a -> a -> JSONFieldGrammar s a
  optionalFieldDefAla fn _pack l def = JsonFG $ \_ cs s ->
    let x = aview l s
     in if x == def
          then mempty
          else jsonField cs fn (toJSON (pack' _pack x))

  freeTextField :: FieldName -> ALens' s (Maybe String) -> JSONFieldGrammar s (Maybe String)
  freeTextField fn l = JsonFG $ \_v cs s ->
    maybe mempty (jsonField cs fn . toJSON) (aview l s)

  freeTextFieldDef :: FieldName -> ALens' s String -> JSONFieldGrammar s String
  freeTextFieldDef fn l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . aview l

  freeTextFieldDefST :: FieldName -> ALens' s ST.ShortText -> JSONFieldGrammar s ST.ShortText
  freeTextFieldDefST = defaultFreeTextFieldDefST

  monoidalFieldAla :: (ToJSON b, Monoid a, Newtype a b) => FieldName -> (a -> b) -> ALens' s a -> JSONFieldGrammar s a
  monoidalFieldAla fn _pack l = JsonFG $ \_v cs ->
    jsonField cs fn . toJSON . pack' _pack . aview l

  prefixedFields :: FieldName -> ALens' s [(String, String)] -> JSONFieldGrammar s [(String, String)]
  prefixedFields _fnPfx l = JsonFG $ \_v _cs s ->
    [Key.fromString n .= v | (n, v) <- aview l s]

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

jsonField :: [Condition ConfVar] -> FieldName -> Value -> [Pair]
jsonField cs fn v
  | v == emptyArray = mempty
  | v == emptyString = mempty
  | null cs = [Key.fromString (fromUTF8BS fn) .= v]
  | otherwise = [Key.fromString (fromUTF8BS fn) .= v']
  where
    v' = object ["if" .= toJSON (foldl1' CAnd cs), "then" .= v]

    -- Should be added to aeson
    emptyString :: Value
    emptyString = String ""

jsonGenericPackageDescription :: GenericPackageDescription -> Value
jsonGenericPackageDescription gpd =
  object $
    concat
      [ jsonPackageDescription v (packageDescription gpd),
        jsonSetupBInfo v (setupBuildInfo (packageDescription gpd)),
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
jsonPackageDescription v pd =
  jsonFieldGrammar v [] packageDescriptionFieldGrammar pd
    ++ ["source-repos" .= jsonSourceRepos v (sourceRepos pd)]

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [Value]
jsonSourceRepos v = map (jsonSourceRepo v)

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> Value
jsonSourceRepo v repo =
  object (jsonFieldGrammar v [] (sourceRepoFieldGrammar kind) repo)
  where
    kind = repoKind repo

jsonSetupBInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [Pair]
jsonSetupBInfo _ Nothing = mempty
jsonSetupBInfo v (Just sbi)
  | defaultSetupDepends sbi = mempty
  | null vs = mempty
  | otherwise = ["custom-setup" .= object vs]
  where
    vs = jsonFieldGrammar v [] (setupBInfoFieldGrammar False) sbi

jsonGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [Pair]
jsonGenPackageFlags v flags
  | null flags = mempty
  | otherwise = ["flags" .= flags']
  where
    flags' =
      object
        [ Key.fromString (unFlagName name) .= object (jsonFieldGrammar v [] (flagFieldGrammar name) flag)
          | flag@(MkPackageFlag name _ _ _) <- flags
        ]

jsonCondLibrary :: CabalSpecVersion -> Maybe (CondTree ConfVar [Dependency] Library) -> [Pair]
jsonCondLibrary _ Nothing = mempty
jsonCondLibrary v (Just condTree) = ["library" .= condTree']
  where
    condTree' = jsonCondTree2 v (libraryFieldGrammar LMainLibName) condTree

jsonCondSubLibraries :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Library)] -> [Pair]
jsonCondSubLibraries v libs
  | null libs = mempty
  | otherwise = ["sub-libraries" .= libs']
  where
    libs' =
      [ KeyMap.insert "name" (fromString $ unUnqualComponentName n) $
          jsonCondTree2 v (libraryFieldGrammar $ LSubLibName n) condTree
        | (n, condTree) <- libs
      ]

jsonCondForeignLibs :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)] -> [Pair]
jsonCondForeignLibs v flibs
  | null flibs = mempty
  | otherwise = ["foreign-libraries" .= flibs']
  where
    flibs' =
      [ KeyMap.insert "name" (fromString $ unUnqualComponentName n) $
          jsonCondTree2 v (foreignLibFieldGrammar n) condTree
        | (n, condTree) <- flibs
      ]

jsonCondExecutables :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [Pair]
jsonCondExecutables v exes
  | null exes = mempty
  | otherwise = ["executables" .= exes']
  where
    exes' =
      [ KeyMap.insert "name" (fromString $ unUnqualComponentName n) $
          jsonCondTree2 v (executableFieldGrammar n) condTree
        | (n, condTree) <- exes
      ]

jsonCondTestSuites :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)] -> [Pair]
jsonCondTestSuites v suites
  | null suites = mempty
  | otherwise = ["test-suites" .= suites']
  where
    suites' =
      [ KeyMap.insert "name" (fromString $ unUnqualComponentName n) $
          jsonCondTree2 v testSuiteFieldGrammar (fmap unvalidateTestSuite condTree)
        | (n, condTree) <- suites
      ]

jsonCondBenchmarks :: CabalSpecVersion -> [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)] -> [Pair]
jsonCondBenchmarks v suites
  | null suites = mempty
  | otherwise = ["benchmarks" .= suites']
  where
    suites' =
      [ KeyMap.insert "name" (fromString $ unUnqualComponentName n) $
          jsonCondTree2 v benchmarkFieldGrammar (fmap unvalidateBenchmark condTree)
        | (n, condTree) <- suites
      ]

jsonCondTree2 :: CabalSpecVersion -> JSONFieldGrammar' s -> CondTree ConfVar [Dependency] s -> KeyMap.KeyMap Value
jsonCondTree2 v grammar = merge . go []
  where
    go cs (CondNode it _ ifs) =
      jsonFieldGrammar v cs grammar it ++ concatMap (jsonIf cs) ifs

    jsonIf cs (CondBranch c thenTree Nothing) =
      go (c : cs) thenTree
    jsonIf cs (CondBranch c thenTree (Just elseTree)) =
      go (c : cs) thenTree ++ go (CNot c : cs) elseTree

    merge :: [Pair] -> KeyMap.KeyMap Value
    merge = fmap toJSON . KeyMap.fromListWith (<>) . map (second (: []))
