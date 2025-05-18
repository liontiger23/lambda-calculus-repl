module LambdaCalculus.REPL
    ( runREPL
    ) where

import System.IO ( hFlush, stdout )
import Control.Monad (unless)
import qualified Data.Text.IO as T
import Data.Text (Text)

-- Runs the Run-Evaluate-Print-Loop with given
-- evaluation function.
runREPL :: Text -> (Text -> Text) -> IO ()
runREPL prompt eval =
  do input <- read'             -- read
     unless (input == ":q" || input == ":quit") $
       do T.putStrLn (eval input) -- evaluate and print
          runREPL prompt eval   -- loop
  where
    read' =
      do T.putStr prompt
         hFlush stdout
         T.getLine
