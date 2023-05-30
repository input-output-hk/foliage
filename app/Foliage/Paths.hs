{-# LANGUAGE TypeFamilies #-}

module Foliage.Paths where

-- let scheme = dropWhileEnd (not . isAlpha) $ uriScheme uri
--
-- let host = maybe (error $ "invalid uri " ++ show uri) uriRegName (uriAuthority uri)
--
-- let path = cacheDir </> joinPath (scheme : host : pathSegments uri)
