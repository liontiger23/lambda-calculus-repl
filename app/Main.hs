module Main (main) where

import LambdaCalculus.REPL ( runREPL )

main :: IO ()
main = runREPL "λ> " id 
