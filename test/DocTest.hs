module Main where

import Prelude.Compat
import Test.DocTest

main :: IO ()
main = doctest
  [ "-XBangPatterns"
  , "-XDeriveFoldable"
  , "-XDeriveGeneric"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XGADTs"
  , "-XInstanceSigs"
  , "-XLambdaCase"
  , "-XMultiParamTypeClasses"
  , "-XNoExtendedDefaultRules"
  , "-XNoImplicitPrelude"
  , "-XOverloadedLists"
  , "-XOverloadedStrings"
  , "-XScopedTypeVariables"
  , "-XTypeFamilies"
  , "src"
  ]
