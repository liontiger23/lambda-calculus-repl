module Main (main) where

import LambdaCalculus.REPL ( runREPL )
import LambdaCalculus.Parser (parseTerm)
import LambdaCalculus.Terms (render)

main :: IO ()
main = runREPL "λ> " (either id render . parseTerm)
