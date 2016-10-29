{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Aeson (Value(..), (.=))
import Data.Aeson.Validation
import Data.Aeson.Validation.Internal
import Data.Generics.Uniplate.Operations (universe)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import Data.Text (Text)
import Data.Text.Arbitrary ()
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Text.Show.Functions ()

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "normalize" $ do
    prop "leaves no SAlts as children of SAlts" $ \s ->
      and [ all notAlts ss
            | SAlts ss <- universe (normalize s) ]

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

notAlts :: Schema -> Bool
notAlts (SAlts _) = False
notAlts _ = True

--------------------------------------------------------------------------------
-- Orphans

arbitrary' :: Arbitrary a => Gen a
arbitrary' = scale (`div` 2) arbitrary

instance Arbitrary Demand where
  arbitrary = (\b -> if b then Opt else Req) <$> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary
  shrink xs =
    shrinkList shrink (NonEmpty.toList xs) >>= \case
      [] -> []
      y:ys -> pure (y :| ys)

instance Arbitrary Schema where
  arbitrary = frequency $
    map (4,)
      [ SObject <$> arbitrary <*> arbitrary'
      , STuple <$> arbitrary'
      , SAlts <$> arbitrary'
      ] ++
    map (5,)
      [ SArray <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary'
      , SNullable <$> arbitrary'
      , SNegate <$> arbitrary'
      ] ++
    map (7,)
      [ pure SBool
      , pure STrue
      , pure SFalse
      , pure SNumber
      , pure SInteger
      , STheInteger <$> arbitrary
      , SSomeNumber <$> arbitrary <*> arbitrary
      , pure SString
      , STheString <$> arbitrary
      , SSomeString <$> arbitrary <*> arbitrary
      , pure SAnything
      ]
  shrink = genericShrink

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

instance Arbitrary ShallowField where
  arbitrary = ShallowField <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Strict where
  arbitrary = (\b -> if b then Strict else NotStrict) <$> arbitrary

instance Arbitrary Unique where
  arbitrary = (\b -> if b then Unique else NotUnique) <$> arbitrary

instance CoArbitrary Scientific where
  coarbitrary s = coarbitrary (base10Exponent) . coarbitrary (coefficient s)

instance CoArbitrary Text where
  coarbitrary t = coarbitrary (Text.unpack t)

deriving instance Generic Demand
deriving instance Generic Schema
deriving instance Generic ShallowField

deriving instance Show Demand
deriving instance Show Schema
deriving instance Show ShallowField
deriving instance Show Strict
deriving instance Show Unique
