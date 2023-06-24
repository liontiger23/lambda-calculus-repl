module LambdaCalculus.REPL
    ( runREPL
    ) where

import System.IO ( hFlush, stdout )
import Control.Monad (unless)

-- Runs the Run-Evaluate-Print-Loop with given
-- evaluation function.
runREPL :: String -> (String -> String) -> IO ()
runREPL prompt eval =
  do input <- read'             -- read
     unless (input == ":q" || input == ":quit") $
       do putStrLn (eval input) -- evaluate and print
          runREPL prompt eval   -- loop
  where
    read' =
      do putStr $ prompt ++ "> "
         hFlush stdout
         getLine
