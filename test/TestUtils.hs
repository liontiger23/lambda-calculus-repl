module TestUtils where

import Test.Tasty.QuickCheck
import LambdaCalculus.Terms

newtype AnyTerm = AnyTerm Term
  deriving Show

instance Arbitrary AnyTerm where
  arbitrary = sized (fmap AnyTerm . arbitrarySizedTerm)

arbitrarySizedTerm :: Int -> Gen Term
arbitrarySizedTerm 0 = Var <$> ident
arbitrarySizedTerm n = oneof
  [ App
    <$> arbitrarySizedTerm (n `div` 2)
    <*> arbitrarySizedTerm (n `div` 2)
  , Abs
    <$> ident
    <*> arbitrarySizedTerm (n `div` 2)
  ]

ident :: Gen Ident
ident = pure <$> elements ['a'..'z']

newtype LambdaPrompt = LambdaPrompt String
  deriving Show

instance Arbitrary LambdaPrompt where
  arbitrary = LambdaPrompt <$> elements ["λ", "\\"]
