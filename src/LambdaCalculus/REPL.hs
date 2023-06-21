module LambdaCalculus.REPL
    ( runREPL
    ) where

import System.IO ( hFlush, stdout )
import Control.Monad (unless)

runREPL :: (String -> String) -> IO ()
runREPL eval = do
  input <- read'
  unless (input == ":q" || input == ":quit") $ do
      putStrLn $ eval input
      runREPL eval
  where
    read' = do
      putStr "λ> "
      hFlush stdout
      getLine
