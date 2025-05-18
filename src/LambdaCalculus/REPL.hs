module LambdaCalculus.REPL
    ( runREPL
    , repl
    ) where

import System.Console.Haskeline

import LambdaCalculus.Parser
import LambdaCalculus.Terms

repl :: IO ()
repl = runInputT defaultSettings { historyFile = Just ".lambda-history" } $
  runREPL "λ> " (either id render . parseTerm)

-- Runs the Run-Evaluate-Print-Loop with given
-- evaluation function.
runREPL :: String -> (String -> String) -> InputT IO ()
runREPL prompt eval =
  do minput <- getInputLine prompt                 -- read
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just ":quit" -> return ()
       Just input ->
         do outputStrLn (eval input) -- evaluate and print
            runREPL prompt eval   -- loop
