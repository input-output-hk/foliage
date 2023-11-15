module Foliage.Rules.HackageExtra (
  hackageExtraRules,
) where

import Development.Shake (Action, Rules, action, copyFileChanged, filePattern, need, (%>))
import Development.Shake.FilePath
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (PackageIdentifier (..), unPackageName)
import Foliage.Meta (PackageVersionSpec (packageVersionRevisions), RevisionSpec (RevisionSpec, revisionNumber))
import Text.Read (readMaybe)

hackageExtraRules
  :: FilePath
  -> (PackageIdentifier -> FilePath)
  -> (PackageIdentifier -> Int -> FilePath)
  -> Action [(FilePath, PackageIdentifier, PackageVersionSpec)]
  -> Rules ()
hackageExtraRules outputDir cabalFileForPkgId cabalFileRevisionForPkgId getPkgSpecs = do
  action $ do
    getPkgSpecs
      >>= need
        . concatMap
          ( \(_, pkgId@PackageIdentifier{pkgName}, pkgSpec) ->
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
        , pkgName pkgId == pkgName' ->
            copyFileChanged (cabalFileForPkgId pkgId) path
      _ -> error $ "The path " ++ path ++ " is not valid."
