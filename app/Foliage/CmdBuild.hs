{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foliage.CmdBuild (cmdBuild) where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.Directory qualified as IO

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Development.Shake (
  Change (..),
  Lint (..),
  RuleResult,
  ShakeOptions (..),
  action,
  addOracleCache,
  addShakeExtra,
  filePattern,
  forP,
  getDirectoryFiles,
  liftIO,
  need,
  newCache,
  shake,
  shakeOptions,
  (%>),
 )
import Development.Shake.Classes (
  Binary,
  Hashable,
  NFData,
  Typeable,
 )
import Development.Shake.FilePath (
  replaceFileName,
  takeDirectory,
  (<.>),
  (</>),
 )
import Distribution.Package (
  PackageId,
  PackageIdentifier (..),
  unPackageName,
 )
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Hackage.Security.Server (FileExpires (..))

import Distribution.Types.Orphans ()
import Foliage.FetchURL (addFetchURLRule)
import Foliage.HackageSecurity (createKeys)
import Foliage.Meta
import Foliage.Meta.Aeson ()
import Foliage.Options
import Foliage.Rules.Core
import Foliage.Rules.HackageExtra
import Foliage.Rules.Pages
import Foliage.SourceDist
import Foliage.Time

cmdBuild :: BuildOptions -> IO ()
cmdBuild buildOptions = do
  let
    inputDir = buildOptsInputDir buildOptions
    outputDir = buildOptsOutputDir buildOptions
    cacheDir = "_cache"

  currentTime <- case buildOptsCurrentTime buildOptions of
    Nothing -> do
      t <- truncateSeconds <$> liftIO getCurrentTime
      putStrLn $ "Current time set to " <> iso8601Show t <> ". You can set a fixed time using the --current-time option."
      return t
    Just t -> do
      putStrLn $ "Current time set to " <> iso8601Show t <> "."
      return t

  let expireSignaturesOn = buildOptsExpireSignaturesOn buildOptions
  for_ expireSignaturesOn $ \time ->
    putStrLn $ "Expiry time set to " <> iso8601Show time

  let
    opts =
      shakeOptions
        { shakeFiles = cacheDir
        , shakeChange = ChangeModtimeAndDigest
        , shakeColor = True
        , shakeLint = Just LintBasic
        , shakeReport = ["report.html", "report.json"]
        , shakeThreads = buildOptsNumThreads buildOptions
        , shakeVerbosity = buildOptsVerbosity buildOptions
        , -- "extra" configuration values, which can be retrived with getShakeExtra
          shakeExtra =
            -- signing options (at type SignOptions)
            addShakeExtra (buildOptsSignOpts buildOptions)
              -- signature expiration (at type FileExpires)
              . addShakeExtra (FileExpires expireSignaturesOn)
              $ mempty
        }

  -- Create keys if needed
  case buildOptsSignOpts buildOptions of
    SignOptsSignWithKeys keysPath -> do
      ks <- IO.doesDirectoryExist keysPath
      unless ks $ do
        putStrLn $ "You don't seem to have created a set of TUF keys. I will create one in " <> keysPath
        createKeys keysPath
    _otherwise -> pure ()

  shake opts $ do
    --
    -- Custom rule to fetch urls
    --
    addFetchURLRule

    --
    -- Read metadata from meta.toml
    --
    packageVersionSpec <- newCache $ \path -> do
      need [path]
      meta <- liftIO $ readPackageVersionSpec path
      validateMeta path meta
      return meta

    --
    -- Functions to map package ids to various locations
    --
    let metaFileForPkgId :: PackageIdentifier -> FilePath
        metaFileForPkgId PackageIdentifier{pkgName, pkgVersion} =
          inputDir </> unPackageName pkgName </> prettyShow pkgVersion </> "meta.toml"

    let cabalFileForPkgId :: PackageIdentifier -> FilePath
        cabalFileForPkgId PackageIdentifier{pkgName, pkgVersion} =
          cacheDir </> unPackageName pkgName </> prettyShow pkgVersion </> unPackageName pkgName <.> "cabal"

    let cabalFileRevisionForPkgId :: PackageIdentifier -> Int -> FilePath
        cabalFileRevisionForPkgId pkgId revNum =
          metaFileForPkgId pkgId `replaceFileName` "revisions" </> show revNum <.> "cabal"

    let sdistPathForPkgId :: PackageIdentifier -> FilePath
        sdistPathForPkgId pkgId =
          outputDir </> "package" </> prettyShow pkgId <.> "tar.gz"

    --
    -- Fetching and preparing the package source code
    --
    -- Note: we use the package cabal file as a proxy for the whole source code.

    cacheDir </> "*/*/*.cabal" %> \path ->
      case filePattern (cacheDir </> "*/*/*.cabal") path of
        Just [name, version, name']
          | Just pkgId <- PackageIdentifier <$> simpleParsec name <*> simpleParsec version
          , name == name' -> do
              let metaFile = metaFileForPkgId pkgId
              pkgSpec <- packageVersionSpec metaFile

              fetchPackageVersion cacheDir (packageVersionSource pkgSpec) (takeDirectory path)

              applyPatches metaFile (takeDirectory path)

              when (packageVersionForce pkgSpec) $ do
                updateCabalFileVersion path (pkgVersion pkgId)
        _ -> error $ "The path " ++ path ++ " is not valid."

    --
    -- Index creation
    --

    getPkgSpecs <- do
      getPkgSpecs' <- addOracleCache $ \PkgSpecs{} -> do
        metaFiles <- getDirectoryFiles inputDir ["*/*/meta.toml"]
        when (null metaFiles) $ do
          error $
            unlines
              [ "We could not find any package metadata file (i.e. _sources/<name>/<version>/meta.toml)"
              , "Make sure you are passing the right input directory. The default input directory is _sources"
              ]

        forP metaFiles $ \path ->
          case filePattern "*/*/meta.toml" path of
            Just [name, version] -> do
              let pkgName = fromMaybe (error $ "invalid package name: " ++ name) $ simpleParsec name
              let pkgVersion = fromMaybe (error $ "invalid package version: " ++ version) $ simpleParsec version
              let pkgId = PackageIdentifier pkgName pkgVersion
              pkgSpec <- packageVersionSpec (inputDir </> path)
              return (inputDir </> path, pkgId, pkgSpec)
            _ -> error "the impossible happened"

      return $ getPkgSpecs' $ PkgSpecs ()

    --
    -- Rules for core repository functionality
    --
    coreRules outputDir cabalFileForPkgId sdistPathForPkgId currentTime getPkgSpecs

    --
    -- Rules for Hackage-like paths, e.g.
    --
    -- package/pkg-id/revision/rev-num.cabal
    -- package/pkg-id/pkg-name.cabal

    hackageExtraRules outputDir cabalFileForPkgId cabalFileRevisionForPkgId getPkgSpecs

    --
    -- Foliage metadata
    --

    action $
      when (buildOptsWriteMetadata buildOptions) $
        need [outputDir </> "foliage/packages.json"]

    outputDir </> "foliage/packages.json" %> \path ->
      getPkgSpecs >>= \pkgSpecs ->
        liftIO $
          Aeson.encodeFile
            path
            [ Aeson.object $
              [ "pkg-name" Aeson..= pkgName pkgId
              , "pkg-version" Aeson..= pkgVersion pkgId
              , "url" Aeson..= packageVersionSourceToUri (packageVersionSource pkgSpec)
              ]
                ++ ["forced-version" Aeson..= True | packageVersionForce pkgSpec]
                ++ (case packageVersionTimestamp pkgSpec of Nothing -> []; Just t -> ["timestamp" Aeson..= t])
            | (_, pkgId, pkgSpec) <- pkgSpecs
            ]

    --
    -- Website pages
    --

    websitePagesRules outputDir currentTime getPkgSpecs metaFileForPkgId packageVersionSpec cabalFileForPkgId

validateMeta :: (MonadFail m) => FilePath -> PackageVersionSpec -> m ()
validateMeta metaFile PackageVersionSpec{..} = do
  case (NE.nonEmpty packageVersionRevisions, packageVersionTimestamp) of
    (Just _someRevisions, Nothing) ->
      fail $
        unlines
          [ metaFile <> " has cabal file revisions but the package has no timestamp."
          , "This combination doesn't make sense. Either add a timestamp on the original package or remove the revisions."
          ]
    (Just (NE.sort -> someRevisions), Just ts)
      -- WARN: this should really be a <=
      | revisionTimestamp (NE.head someRevisions) < ts ->
          fail $
            unlines
              [ metaFile <> " has a revision with timestamp earlier than the package itself."
              , "Adjust the timestamps so that all revisions come after the package publication."
              ]
      | not (null $ duplicates (revisionTimestamp <$> someRevisions)) ->
          fail $
            unlines
              [ metaFile <> " has two revisions entries with the same timestamp."
              , "Adjust the timestamps so that all the revisions happen at a different time."
              ]
    _otherwise ->
      return ()

  case (NE.nonEmpty packageVersionDeprecations, packageVersionTimestamp) of
    (Just _someDeprecations, Nothing) ->
      fail $
        unlines
          [ metaFile <> " has deprecations but the package has no timestamp."
          , "This combination doesn't make sense. Either add a timestamp on the original package or remove the deprecation."
          ]
    (Just (NE.sort -> someDeprecations), Just ts)
      | deprecationTimestamp (NE.head someDeprecations) <= ts ->
          fail $
            unlines
              [ metaFile <> " has a deprecation entry with timestamp earlier (or equal) than the package itself."
              , "Adjust the timestamps so that all the (un-)deprecations come after the package publication."
              ]
      | not (deprecationIsDeprecated (NE.head someDeprecations)) ->
          fail $
            "The first deprecation entry in" <> metaFile <> " cannot be an un-deprecation"
      | not (null $ duplicates (deprecationTimestamp <$> someDeprecations)) ->
          fail $
            unlines
              [ metaFile <> " has two deprecation entries with the same timestamp."
              , "Adjust the timestamps so that all the (un-)deprecations happen at a different time."
              ]
      | not (null $ doubleDeprecations someDeprecations) ->
          fail $
            unlines
              [ metaFile <> " contains two consecutive deprecations or two consecutive un-deprecations."
              , "Make sure deprecations and un-deprecations alternate in time."
              ]
    _otherwise ->
      return ()
 where
  duplicates :: (Ord a) => NE.NonEmpty a -> [a]
  duplicates = mapMaybe (listToMaybe . NE.tail) . NE.group

  doubleDeprecations :: NE.NonEmpty DeprecationSpec -> [NE.NonEmpty DeprecationSpec]
  doubleDeprecations = filter ((> 1) . length) . NE.groupWith deprecationIsDeprecated

newtype PkgSpecs = PkgSpecs () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult PkgSpecs = [(FilePath, PackageId, PackageVersionSpec)]
