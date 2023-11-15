module Foliage.Rules.HackageExtra (
  hackageExtraRules,
) where

import Text.Read (readMaybe)

import Data.Map.Strict qualified as M
import Development.Shake (Action, Rules, action, askOracle, copyFileChanged, filePattern, need, (%>))
import Development.Shake.FilePath
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageIdentifier (..), unPackageName)

import Foliage.Meta (PackageVersionSpec (..), RevisionSpec (..), latestRevisionNumber)
import Foliage.Rules.Utils

hackageExtraRules :: FilePath -> Rules ()
hackageExtraRules outputDir = do
  action $
    askOracle PkgSpecs
      >>= need
        . M.foldMapWithKey
          ( \pkgId@PackageIdentifier{pkgName} (_, pkgSpec) ->
              let baseDir = outputDir </> "package" </> prettyShow pkgId
               in (baseDir </> unPackageName pkgName <.> "cabal")
                    : (baseDir </> "revision" </> "0.cabal")
                    : [ baseDir </> "revision" </> show revisionNumber <.> "cabal"
                      | RevisionSpec{revisionNumber} <- packageVersionRevisions pkgSpec
                      ]
          )

  outputDir </> "package/*/revision/*.cabal" %> \path ->
    case filePattern (outputDir </> "package/*/revision/*.cabal") path of
      Just [pkgIdStr, revNumStr]
        | Just pkgId <- simpleParsec pkgIdStr
        , Just revNum <- readMaybe revNumStr ->
            if revNum == 0
              then copyFileChanged (cabalFileForPkgId pkgId) path
              else copyFileChanged (cabalFileRevisionForPkgId pkgId revNum) path
      _ -> error $ "The path " ++ path ++ " is not valid."

  outputDir </> "package/*/*.cabal" %> \path ->
    case filePattern (outputDir </> "package/*/*.cabal") path of
      Just [pkgIdStr, pkgNameStr]
        | Just pkgId <- simpleParsec pkgIdStr
        , Just pkgName' <- simpleParsec pkgNameStr
        , pkgName pkgId == pkgName' -> do
            pkgSpec <- pkgSpecForPkgId pkgId
            case latestRevisionNumber pkgSpec of
              Nothing -> copyFileChanged (cabalFileForPkgId pkgId) path
              Just revNum -> copyFileChanged (cabalFileRevisionForPkgId pkgId revNum) path
      _ -> error $ "The path " ++ path ++ " is not valid."
