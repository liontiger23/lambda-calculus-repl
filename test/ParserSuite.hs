module ParserSuite (parserTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import TestUtils
import LambdaCalculus.Parser (parseTerm)
import LambdaCalculus.Terms

import Data.Functor ((<&>))

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testGroup "Properties"
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
  , testGroup "Samples" $ samples <&> \(str, term) -> testCase str $
      let res = parseTerm str
      in  assertEqual (either id render res) (Right term) res
  ]

samples :: [(String, Term)]
samples =
  [ ("x",
     Var "x")
  , ("(x)",
     Var "x")
  , ("((x))",
     Var "x")

  , (lambda ++ "x.x",
     Abs "x" (Var "x"))
  , ("(" ++ lambda ++ "x.x" ++ ")",
     Abs "x" (Var "x"))

  , (lambda ++ "x." ++ lambda ++ "y.x",
     Abs "x" (Abs "y" (Var "x")))

  , (lambda ++ "x." ++ lambda ++ "y.y",
     Abs "x" (Abs "y" (Var "y")))

  , ("x y",
     App (Var "x") (Var "y"))
  , ("(x y)",
     App (Var "x") (Var "y"))
  , ("(x (y))",
     App (Var "x") (Var "y"))

  , ("x y z",
     App (App (Var "x") (Var "y")) (Var "z"))
  , ("(x y) z",
     App (App (Var "x") (Var "y")) (Var "z"))
  , ("((x y) z)",
     App (App (Var "x") (Var "y")) (Var "z"))

  , ("x (y z)",
     App (Var "x") (App (Var "y") (Var "z")))
  , ("(x (y z))",
     App (Var "x") (App (Var "y") (Var "z")))

  , (lambda ++ "x.x x",
     Abs "x" (App (Var "x") (Var "x")))
  , ("(" ++ lambda ++ "x.x x" ++ ") (" ++ lambda ++ "x.x x" ++ ")",
     App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (App (Var "x") (Var "x"))))
  ]




