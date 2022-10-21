{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Pages
  ( indexPageTemplate,
    packageVersionPageTemplate,
  )
where

import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)

indexPageTemplate :: Template
indexPageTemplate = $(compileMustacheDir "index" "templates")

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
