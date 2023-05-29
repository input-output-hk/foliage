{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Aeson where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.List (foldl1')
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
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
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.Path
import Distribution.Utils.ShortText qualified as ST
import Language.Haskell.Extension

-- Note: this JSONFieldGrammar is not quite general purpose.
--
-- To help with the rendering of conditional dependencies, here we "push"
-- all the conditionals down.
-- So while the build-dependencies in a GenericPackageDescription could
-- be represented as:
--
-- {
--   "build-depends": ["a", "b", "c"],
--   "conditionals": [{
--       "if": {"os": "darwin"},
--       "then": {
--         "build-depends": ["d"]
--       }
--   }]
-- }
--
-- we decide to represent them as
--
-- {
--   "build-depends": [
--     "a",
--     "b",
--     "c",
--     { "if": "os(darwin)", "then": "d" }
--   ]
-- }
--
-- Note: we also pretty-print the condition.

newtype JSONFieldGrammar s a = JsonFG
  { runJSONFieldGrammar :: CabalSpecVersion -> [Condition ConfVar] -> s -> [Pair]
  }
  deriving (Functor)

type JSONFieldGrammar' s = JSONFieldGrammar s s

jsonFieldGrammar :: CabalSpecVersion -> [Condition ConfVar] -> JSONFieldGrammar s a -> s -> [Pair]
jsonFieldGrammar v cs fg = runJSONFieldGrammar fg v cs

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
    v' = object ["if" .= showCondition (foldl1' cAnd cs), "then" .= v]

    -- Should be added to aeson
    emptyString :: Value
    emptyString = String ""

jsonGenericPackageDescription :: GenericPackageDescription -> Value
jsonGenericPackageDescription gpd = jsonGenericPackageDescription' v gpd
  where
    v = specVersion $ packageDescription gpd

jsonGenericPackageDescription' :: CabalSpecVersion -> GenericPackageDescription -> Value
jsonGenericPackageDescription' v gpd =
  object $
    concat
      [ jsonPackageDescription v (packageDescription gpd),
        jsonSetupBuildInfo v (setupBuildInfo (packageDescription gpd)),
        jsonGenPackageFlags v (genPackageFlags gpd),
        jsonCondLibrary v (condLibrary gpd),
        jsonCondSubLibraries v (condSubLibraries gpd),
        jsonCondForeignLibs v (condForeignLibs gpd),
        jsonCondExecutables v (condExecutables gpd),
        jsonCondTestSuites v (condTestSuites gpd),
        jsonCondBenchmarks v (condBenchmarks gpd)
      ]

jsonPackageDescription :: CabalSpecVersion -> PackageDescription -> [Pair]
jsonPackageDescription v pd@PackageDescription {sourceRepos, setupBuildInfo} =
  jsonFieldGrammar v [] packageDescriptionFieldGrammar pd
    <> jsonSourceRepos v sourceRepos
    <> jsonSetupBuildInfo v setupBuildInfo

jsonSourceRepos :: CabalSpecVersion -> [SourceRepo] -> [Pair]
jsonSourceRepos v =
  concatMap (\neRepos -> ["source-repository" .= NE.map (jsonSourceRepo v) neRepos]) . NE.nonEmpty

jsonSourceRepo :: CabalSpecVersion -> SourceRepo -> Value
jsonSourceRepo v repo@SourceRepo {repoKind} =
  object $ jsonFieldGrammar v [] (sourceRepoFieldGrammar repoKind) repo

jsonSetupBuildInfo :: CabalSpecVersion -> Maybe SetupBuildInfo -> [Pair]
jsonSetupBuildInfo v =
  concatMap (\sbi -> ["custom-setup" .= jsonFieldGrammar v [] (setupBInfoFieldGrammar False) sbi])

jsonGenPackageFlags :: CabalSpecVersion -> [PackageFlag] -> [Pair]
jsonGenPackageFlags v =
  concatMap (\neFlags -> ["flags" .= object (NE.toList $ NE.map (jsonFlag v) neFlags)]) . NE.nonEmpty

jsonFlag :: CabalSpecVersion -> PackageFlag -> Pair
jsonFlag v flag@(MkPackageFlag name _ _ _) =
  Key.fromString (unFlagName name) .= object (jsonFieldGrammar v [] (flagFieldGrammar name) flag)

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

jsonCondTree :: forall s. CabalSpecVersion -> JSONFieldGrammar' s -> CondTree ConfVar [Dependency] s -> [Pair]
jsonCondTree v grammar = go []
  where
    go cs (CondNode it _ ifs) =
      KeyMap.toList $ foldr merge (KeyMap.fromList $ jsonFieldGrammar v cs grammar it) $ concatMap (jsonIf cs) ifs

    jsonIf :: [Condition ConfVar] -> CondBranch ConfVar c s -> [Pair]
    jsonIf cs (CondBranch c thenTree Nothing) =
      go (c : cs) thenTree
    jsonIf cs (CondBranch c thenTree (Just elseTree)) =
      go (c : cs) thenTree ++ go (cNot c : cs) elseTree

    merge :: Pair -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
    merge = uncurry $ KeyMap.insertWith $ \new ->
      \case
        (Array a) -> Array (a `V.snoc` new)
        old -> Array (V.fromList [old, new])

withName :: (ToJSON v) => v -> [Pair] -> Value
withName n s = object $ ("name" .= n) : s

showCondition :: Condition ConfVar -> String
showCondition (Var x) = showConfVar x
showCondition (Lit b) = show b
showCondition (CNot c) = "!" <> showCondition c
showCondition (COr c1 c2) = "(" <> unwords [showCondition c1, "||", showCondition c2] <> ")"
showCondition (CAnd c1 c2) = "(" <> unwords [showCondition c1, "&&", showCondition c2] <> ")"

showConfVar :: ConfVar -> String
showConfVar (OS os) = "os(" <> prettyShow os <> ")"
showConfVar (Arch arch) = "arch(" <> prettyShow arch <> ")"
showConfVar (PackageFlag name) = "flag(" <> unFlagName name <> ")"
showConfVar (Impl c v) = "impl(" <> prettyShow c <> " " <> prettyShow v <> ")"

showIfCondition :: Condition ConfVar -> String
showIfCondition c = "if " <> showCondition c

newtype ViaPretty a = ViaPretty a

instance (Pretty a) => ToJSON (ViaPretty a) where
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

deriving via (ViaUnpack (List sep b a)) instance (ToJSON a) => ToJSON (List sep b a)

deriving via (ViaPretty (SymbolicPath from to)) instance ToJSON (SymbolicPath from to)

deriving via (ViaPretty BuildType) instance ToJSON BuildType

deriving via (ViaPretty PackageName) instance ToJSON PackageName

deriving via (ViaPretty Version) instance ToJSON Version

instance ToJSON RepoType

instance ToJSON KnownRepoType

deriving via (ViaPretty Extension) instance ToJSON Extension

deriving via (ViaPretty Language) instance ToJSON Language

deriving via (ViaUnpack (MQuoted a)) instance (ToJSON a) => ToJSON (MQuoted a)

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
