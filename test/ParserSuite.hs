module ParserSuite where

import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils
import LambdaCalculus.Parser (parseTerm)
import LambdaCalculus.Terms (render', render)

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
        \(AnyTerm t) ->
          let str = render t -- TODO: add random spaces
          in  either id render (parseTerm str) === render t
  ]

