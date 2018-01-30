module Main where

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
  , "-XOverloadedLists"
  , "-XOverloadedStrings"
  , "-XScopedTypeVariables"
  , "-XTypeFamilies"
  , "src"
  ]
