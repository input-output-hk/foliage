module Foliage.TUF where

import Development.Shake
import Foliage.HackageSecurity (
  computeFileInfoSimple,
  writeSignedJSON,
 )
import Foliage.Oracles (
  ExpiryTime (..),
  anchorRepoPath',
  readKeys,
 )
import Hackage.Security.Key.Env (
  fromKeys,
 )
import Hackage.Security.Server (
  FileExpires (..),
  FileVersion (..),
  KeyThreshold (..),
  Mirrors (..),
  RepoLayout (..),
  RoleSpec (..),
  Root (..),
  RootRoles (..),
  Snapshot (..),
  Timestamp (..),
  somePublicKey,
 )
import Hackage.Security.Util.Path qualified as Sec

mkMirrors :: FilePath -> Action ()
mkMirrors path = do
  expiryTime <- askOracle ExpiryTime
  privateKeysMirrors <- readKeys "mirrors"
  liftIO $
    writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysMirrors $
      Mirrors
        { mirrorsVersion = FileVersion 1
        , mirrorsExpires = FileExpires expiryTime
        , mirrorsMirrors = []
        }

mkRoot :: FilePath -> Action ()
mkRoot path = do
  expiryTime <- askOracle ExpiryTime

  privateKeysRoot <- readKeys "root"
  privateKeysTarget <- readKeys "target"
  privateKeysSnapshot <- readKeys "snapshot"
  privateKeysTimestamp <- readKeys "timestamp"
  privateKeysMirrors <- readKeys "mirrors"

  liftIO $
    writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysRoot $
      Root
        { rootVersion = FileVersion 1
        , rootExpires = FileExpires expiryTime
        , rootKeys =
            fromKeys $
              concat
                [ privateKeysRoot
                , privateKeysTarget
                , privateKeysSnapshot
                , privateKeysTimestamp
                , privateKeysMirrors
                ]
        , rootRoles =
            RootRoles
              { rootRolesRoot =
                  RoleSpec
                    { roleSpecKeys = map somePublicKey privateKeysRoot
                    , roleSpecThreshold = KeyThreshold 2
                    }
              , rootRolesSnapshot =
                  RoleSpec
                    { roleSpecKeys = map somePublicKey privateKeysSnapshot
                    , roleSpecThreshold = KeyThreshold 1
                    }
              , rootRolesTargets =
                  RoleSpec
                    { roleSpecKeys = map somePublicKey privateKeysTarget
                    , roleSpecThreshold = KeyThreshold 1
                    }
              , rootRolesTimestamp =
                  RoleSpec
                    { roleSpecKeys = map somePublicKey privateKeysTimestamp
                    , roleSpecThreshold = KeyThreshold 1
                    }
              , rootRolesMirrors =
                  RoleSpec
                    { roleSpecKeys = map somePublicKey privateKeysMirrors
                    , roleSpecThreshold = KeyThreshold 1
                    }
              }
        }

mkSnapshot :: FilePath -> Action ()
mkSnapshot path = do
  expiryTime <- askOracle ExpiryTime
  privateKeysSnapshot <- readKeys "snapshot"

  rootInfo <- anchorRepoPath' repoLayoutRoot >>= computeFileInfoSimple
  mirrorsInfo <- anchorRepoPath' repoLayoutMirrors >>= computeFileInfoSimple
  tarInfo <- anchorRepoPath' repoLayoutIndexTar >>= computeFileInfoSimple
  tarGzInfo <- anchorRepoPath' repoLayoutIndexTarGz >>= computeFileInfoSimple

  liftIO $
    writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysSnapshot $
      Snapshot
        { snapshotVersion = FileVersion 1
        , snapshotExpires = FileExpires expiryTime
        , snapshotInfoRoot = rootInfo
        , snapshotInfoMirrors = mirrorsInfo
        , snapshotInfoTar = Just tarInfo
        , snapshotInfoTarGz = tarGzInfo
        }

mkTimestamp :: FilePath -> Action ()
mkTimestamp path = do
  expiryTime <- askOracle ExpiryTime
  privateKeysTimestamp <- readKeys "timestamp"

  snapshotInfo <- anchorRepoPath' repoLayoutSnapshot >>= computeFileInfoSimple

  liftIO $
    writeSignedJSON (Sec.Path path :: Sec.Path Sec.Relative) privateKeysTimestamp $
      Timestamp
        { timestampVersion = FileVersion 1
        , timestampExpires = FileExpires expiryTime
        , timestampInfoSnapshot = snapshotInfo
        }
