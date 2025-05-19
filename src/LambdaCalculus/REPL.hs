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
  repl (lambda ++ "> ") evalTerm

runREPL :: REPL a -> IO a
runREPL r = runInputT defaultSettings { historyFile = Just ".lambda-history" } $
  evalStateT r M.empty

-- Runs the Run-Evaluate-Print-Loop with given
-- evaluation function.
repl :: String -> (String -> [String]) -> REPL ()
repl prompt eval =
  do minput <- lift $ getInputLine prompt -- read
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just ":quit" -> return ()
       Just input ->
         do lift $ traverse_ outputStrLn (eval input) -- evaluate and print
            repl prompt eval -- loop

evalTerm :: String -> [String]
evalTerm = either pure (reduceTerm "~ " 100) . parseTerm

reduceTerm :: String -> Int -> Term -> [String]
reduceTerm sep n = fmap ((sep ++) . render) . take n . reduceFully
