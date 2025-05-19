module LambdaCalculus.REPL
    ( runREPL
    , repl
    ) where

import System.Console.Haskeline

import LambdaCalculus.Parser
import LambdaCalculus.Terms
import Data.Foldable (traverse_)

repl :: IO ()
repl = runInputT defaultSettings { historyFile = Just ".lambda-history" } $
  runREPL (lambda ++ "> ") evalTerm

-- Runs the Run-Evaluate-Print-Loop with given
-- evaluation function.
runREPL :: String -> (String -> [String]) -> InputT IO ()
runREPL prompt eval =
  do minput <- getInputLine prompt                 -- read
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just ":quit" -> return ()
       Just input ->
         do traverse_ outputStrLn (eval input) -- evaluate and print
            runREPL prompt eval   -- loop

evalTerm :: String -> [String]
evalTerm = either pure (reduceTerm "~ " 100) . parseTerm

reduceTerm :: String -> Int -> Term -> [String]
reduceTerm sep n = fmap ((sep ++) . render) . take n . reduceFully
