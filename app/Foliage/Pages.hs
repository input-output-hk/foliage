{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foliage.Pages (packageVersionPageTemplate) where

import Text.Mustache (Template)
import Text.Mustache.Compile.TH (compileMustacheDir)

packageVersionPageTemplate :: Template
packageVersionPageTemplate = $(compileMustacheDir "packageVersion" "templates")
