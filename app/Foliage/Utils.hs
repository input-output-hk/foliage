module Foliage.Utils
  ( urlToFileName,
    fileNameToUrl,
  )
where

import Data.Text qualified as T
import Data.Text.Encoding.Base64.URL qualified as T

urlToFileName :: String -> FilePath
urlToFileName = T.unpack . T.encodeBase64Unpadded . T.pack

fileNameToUrl :: FilePath -> String
fileNameToUrl = T.unpack . T.decodeBase64Lenient . T.pack
