{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Foliage.Config (Config (..), Source (..), readConfig)
import Foliage.Options (Options (..), parseOptions)
import Foliage.RepoToolWrapper
import Shelly
import System.FilePath (replaceDirectory, stripExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

cabal :: Text -> [Text] -> Sh ()
cabal = command1_ "cabal" []

main :: IO ()
main = do
  Options {optionsConfig, optionsKeys} <- parseOptions

  eConfig <- readConfig optionsConfig

  case eConfig of
    Left e ->
      hPutStrLn stderr e
    Right config ->
      makeRepository (configSources config) optionsKeys

makeRepository :: MonadIO m => [Source] -> FilePath -> m ()
makeRepository sources keysPath = shelly $ do
  outDir <- absPath "_repo"
  idxDir <- absPath "_repo/index"
  pkgDir <- absPath "_repo/package"

  -- clean repository directory
  rm_rf outDir
  mkdir outDir
  mkdir pkgDir

  keysDir <- absPath keysPath
  ensureKeys keysDir

  forM_ sources $ processSource pkgDir

  echo "Bootstrapping repository"
  liftIO $ bootstrapRepo keysDir outDir

  revisions <-
    fmap catMaybes $ do
      pkgs <- ls pkgDir
      forM pkgs $ \pkg -> do
        let Just pkgId = stripExtension ".tar.gz" pkg
        let revisionPath = replaceDirectory (pkgId <.> "cabal") "revisions"
        hasRevision <- test_e revisionPath
        return $
          if hasRevision
            then Just revisionPath
            else Nothing

  forM_ revisions $ \revisionFilePath -> do
    let Just pkgId = stripExtension ".cabal" $ takeFileName revisionFilePath
    let (pn, pv) = parsePkgId (toTextIgnore pkgId)
    let cabalFilePath = idxDir </> pn </> pv </> (pn <.> "cabal")
    echo $ toTextIgnore cabalFilePath
    echo $ "Adopting revision " <> toTextIgnore revisionFilePath
    cp revisionFilePath cabalFilePath

  unless (null revisions) $ do
    echo "Updating index after applying revisions"
    liftIO $ updateRepo keysDir outDir

  echo $ "Hackage repository built in " <> toTextIgnore outDir

parsePkgId :: Text -> (Text, Text)
parsePkgId t = (T.init pn, pv)
  where
    (pn, pv) = T.breakOnEnd "-" t

ensureKeys :: FilePath -> Sh ()
ensureKeys keysDir = do
  b <- test_d keysDir
  if b
    then echo $ "Using existing keys in " <> toTextIgnore keysDir
    else do
      echo $ "Creating new repository keys in " <> toTextIgnore keysDir
      liftIO $ createKeys keysDir

processSource :: FilePath -> Source -> Sh ()
processSource pkgDir (Source url subdirs) = do
  echo $ "Processing " <> url
  withTmpDir $ \tmpDir -> do
    bash_ "curl" ["--silent", "-L", url, " | tar xz -C ", toTextIgnore tmpDir]

    dir <- skipSingleDirectory tmpDir

    chdir dir $ do
      removeCabalProjectFiles

      case subdirs of
        [] ->
          sdistWithProtection pkgDir
        _ ->
          forM_ subdirs $ \subdir ->
            chdir (fromText subdir) $
              sdistWithProtection pkgDir

sdistWithProtection :: FilePath -> Sh ()
sdistWithProtection pkgDir =
  withTmpDir $ \tmpDir -> do
    print_stdout False $ cabal "sdist" ["-o", toTextIgnore tmpDir]
    [sdistPath] <- ls tmpDir
    let destPath = replaceDirectory sdistPath pkgDir
    -- this is a bit rude
    False <- test_e destPath
    echo $ "written " <> toTextIgnore destPath
    cp sdistPath destPath

removeCabalProjectFiles :: Sh ()
removeCabalProjectFiles = do
  cpfs <- findWhen (\p -> pure $ "./cabal.project" `isPrefixOf` p) "."
  forM_ cpfs $ \p -> do
    echo $ "removing " <> toTextIgnore p
    rm p

skipSingleDirectory :: FilePath -> Sh FilePath
skipSingleDirectory dir = do
  es <- ls dir
  case es of
    [e] -> do
      b <- test_d e
      return $
        if b
          then dir </> e
          else dir
    _ -> return dir
