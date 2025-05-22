module LambdaCalculus.REPL
    ( replMain
    ) where

import System.Console.Haskeline

import LambdaCalculus.Parser
import LambdaCalculus.Terms
import Data.Foldable (traverse_)
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)

type REPL = StateT (Map Ident Term) (InputT IO)

replMain :: IO ()
replMain = runREPL $
  repl (lambda ++ "> ")

runREPL :: REPL a -> IO a
runREPL r = runInputT defaultSettings { historyFile = Just ".lambda-history" } $
  evalStateT r M.empty

-- Runs the Run-Evaluate-Print-Loop
repl :: String -> REPL ()
repl prompt =
  do minput <- lift $ getInputLine prompt
     case minput of
       Nothing -> pure ()
       Just ":q" -> pure ()
       Just ":quit" -> pure ()
       Just ":show" -> printDefs *> repl prompt
       Just input   -> process input *> repl prompt

printDefs :: REPL ()
printDefs = do
  defs <- get
  lift $ forM_ (M.assocs defs) $ \(k, v) ->
    outputStrLn $ k ++ " = " ++ render v

process :: String -> REPL ()
process input
  | '=' `elem` input = definition input
  | otherwise        = eval input

definition :: String -> REPL ()
definition input = case parseTermDef input of
  Left err -> lift $ outputStrLn err
  Right (n, t) -> modify (M.insert n t)

eval :: String -> REPL ()
eval input = lift $ traverse_ outputStrLn (evalTerm input)

evalTerm :: String -> [String]
evalTerm = either pure (reduceTerm "~ " 100) . parseTerm

reduceTerm :: String -> Int -> Term -> [String]
reduceTerm sep n = fmap ((sep ++) . render) . take n . reduceFully
