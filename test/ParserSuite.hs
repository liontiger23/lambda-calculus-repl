module ParserSuite (parserTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils
import LambdaCalculus.Parser (parseTerm)
import LambdaCalculus.Terms (render', renderTokens')

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testProperty "parseTerm . render = id" $
      withMaxSuccess 1000 $
        \(AnyTerm t, LambdaPrompt l) ->
          let res = parseTerm (render' l t)
          in counterexample (either id (render' l) res) $
            res === Right t
  , testProperty "render . parseTerm = id" $
      withMaxSuccess 1000 $
        \(AnyTerm t, LambdaPrompt l, Blind (Paddings paddings)) ->
          let str = concat $ zipWith (++) paddings $ renderTokens' l t -- TODO: add random spaces
              res = parseTerm str
          in counterexample (either id (render' l) res) $
            either id (render' l) res === render' l t
  ]

