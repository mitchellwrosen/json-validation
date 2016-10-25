module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XBangPatterns"
  , "-XDeriveAnyClass"
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
  , "-XTypeFamilies"
  , "src"
  ]
