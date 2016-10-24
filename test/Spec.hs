{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Aeson (Value(..), (.=))
import Data.Aeson.Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "object'" $ do
    it "singleton paths work" $ do
      validate
        (object' ["foo" .: anything, "bar" .: anything])
        (Object ["foo" .= Null, "bar" .= Null])
      `shouldBe` []

    it "paths are flattened out" $ do
      validate
        (object' [["foo","bar"] .: anything])
        (Object ["foo" .= Object ["bar" .= Null]])
      `shouldBe` []

    it "paths merge together" $ do
      validate
        (object' [["foo","bar"] .: bool, ["foo","baz"] .: bool])
        (Object ["foo" .= Object ["bar" .= Bool True, "baz" .= Bool False]])
      `shouldBe` []
