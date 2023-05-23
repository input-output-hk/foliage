{-# LANGUAGE OverloadedStrings #-}

-- | Utilities to implement cabal @v2-sdist@.
--
-- NOTE: copied from cabal-install-3.10.1.0 it should not change but it could
module Distribution.Client.SrcDist
  ( packageDirToSdist,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (when)
import Control.Monad.State.Lazy (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer.Lazy (WriterT, execWriterT, tell)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.List (nub, sort)
import Data.Set qualified as Set
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.SrcDist (listPackageSourcesWithDie)
import Distribution.Simple.Utils (die')
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Verbosity (Verbosity)
import System.FilePath (normalise, takeDirectory, (</>))

-- | Create a tarball for a package in a directory
packageDirToSdist ::
  Verbosity ->
  -- | read in GPD
  GenericPackageDescription ->
  -- | directory containing that GPD
  FilePath ->
  -- | resulting sdist tarball
  IO BSL.ByteString
packageDirToSdist verbosity gpd dir = do
  let thisDie :: Verbosity -> String -> IO a
      thisDie v s = die' v $ "sdist of " <> prettyShow (packageId gpd) ++ ": " ++ s

  files' <- listPackageSourcesWithDie verbosity thisDie dir (flattenPackageDescription gpd) knownSuffixHandlers
  let files :: [FilePath]
      files = nub $ sort $ map normalise files'

  let entriesM :: StateT (Set.Set FilePath) (WriterT [Tar.Entry] IO) ()
      entriesM = do
        let prefix = prettyShow (packageId gpd)
        modify (Set.insert prefix)
        case Tar.toTarPath True prefix of
          Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
          Right path -> tell [Tar.directoryEntry path]

        for_ files $ \file -> do
          let fileDir = takeDirectory (prefix </> file)
          needsEntry <- gets (Set.notMember fileDir)

          when needsEntry $ do
            modify (Set.insert fileDir)
            case Tar.toTarPath True fileDir of
              Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
              Right path -> tell [Tar.directoryEntry path]

          contents <- liftIO . fmap BSL.fromStrict . BS.readFile $ dir </> file
          case Tar.toTarPath False (prefix </> file) of
            Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
            Right path -> tell [(Tar.fileEntry path contents) {Tar.entryPermissions = Tar.ordinaryFilePermissions}]

  entries <- execWriterT (evalStateT entriesM mempty)
  let -- Pretend our GZip file is made on Unix.
      normalize bs = BSL.concat [pfx, "\x03", rest']
        where
          (pfx, rest) = BSL.splitAt 9 bs
          rest' = BSL.tail rest
      -- The Unix epoch, which is the default value, is
      -- unsuitable because it causes unpacking problems on
      -- Windows; we need a post-1980 date. One gigasecond
      -- after the epoch is during 2001-09-09, so that does
      -- nicely. See #5596.
      setModTime :: Tar.Entry -> Tar.Entry
      setModTime entry = entry {Tar.entryTime = 1000000000}
  return . normalize . GZip.compress . Tar.write $ fmap setModTime entries
