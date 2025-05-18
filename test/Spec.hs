import Test.Tasty

import ParserSuite

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  ]
