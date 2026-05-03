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
import Data.Char (isSpace)
import Control.Exception (IOException, try)
import Text.Read (readMaybe)

-- * Options

type Defs = Map Ident Term
data REPLOptions = REPLOptions
  { replDefs :: Defs
  , replDepth :: Int
  }

defaultOptions :: REPLOptions
defaultOptions = REPLOptions M.empty 100

-- * REPL

type REPL = StateT REPLOptions (InputT IO)

replMain :: IO ()
replMain = runREPL $
  repl (lambda ++ "> ")

runREPL :: REPL a -> IO a
runREPL r = runInputT defaultSettings { historyFile = Just ".lambda-history" } $
  evalStateT r defaultOptions

-- Runs the Run-Evaluate-Print-Loop
repl :: String -> REPL ()
repl prompt =
  do minput <- lift $ getInputLine prompt
     case minput of
       Nothing -> pure ()
       Just (':' : cmd) -> command cmd
       Just input   -> process input *> repl prompt
 where
  command :: String -> REPL ()
  command "q" = pure ()
  command "quit" = pure ()
  command "defs" = printDefs *> repl prompt
  command "depth" = printDepth *> repl prompt
  command cmd = (case break isSpace cmd of
      ("save", name) -> writeDefs (trim name)
      ("load", name) -> readDefs (trim name)
      ("depth", dep) -> case readMaybe dep of
        Just d -> putDepth d *> printDepth
        Nothing -> lift $ outputStrLn ("Unexpected depth value: '" <> dep <> "'")
      _              -> lift $ outputStrLn ("Unknown command: " ++ cmd)
    ) *> repl prompt

getDepth :: REPL Int
getDepth = gets replDepth

putDepth :: Int -> REPL ()
putDepth d = modify (\o -> o { replDepth = d })

getDefs :: REPL Defs
getDefs = gets replDefs

modifyDefs :: (Defs -> Defs) -> REPL ()
modifyDefs f = modify (\o -> o { replDefs = f (replDefs o) })

printDepth :: REPL ()
printDepth = getDepth >>= lift . outputStrLn . show

printDefs :: REPL ()
printDefs = showDefs >>= lift . traverse_ outputStrLn

writeDefs :: FilePath -> REPL ()
writeDefs name = showDefs >>= writeFileLines name

readDefs :: FilePath -> REPL ()
readDefs name = readFileLines name >>= traverse_ definition

showDefs :: REPL [String]
showDefs = fmap showDef . M.assocs <$> getDefs

showDef :: (Ident, Term) -> String
showDef (n, t) = n ++ " = " ++ render t

process :: String -> REPL ()
process input
  | '=' `elem` input = definition input
  | otherwise        = eval input

definition :: String -> REPL ()
definition input = case parseTermDef input of
  Left err -> lift $ outputStrLn err
  Right (n, t) -> modifyDefs (M.insert n t)

unwrap :: Term -> REPL Term
unwrap t = do
  subs <- M.assocs <$> getDefs
  let t' = foldr (uncurry substitute) t subs
  if t' == t then pure t else unwrap t'

eval :: String -> REPL ()
eval input = case parseTerm input of
  Left err -> lift $ outputStrLn err
  Right t  -> do
    d <- getDepth
    t' <- unwrap t
    lift $ traverse_ outputStrLn (reduceTerm "~ " d t')

reduceTerm :: String -> Int -> Term -> [String]
reduceTerm sep n = fmap ((sep ++) . render) . take n . reduceFully

-- * Utility

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

readFileLines :: FilePath -> REPL [String]
readFileLines name = do
  res <- lift $ lift $ try (readFile name)
  case res of
    Left e -> do
      lift $ outputStrLn $ show (e :: IOException)
      pure []
    Right s -> pure $ lines s
  
writeFileLines :: FilePath -> [String] -> REPL ()
writeFileLines name ls = do
  res <- lift $ lift $ try (writeFile name (unlines ls))
  case res of
    Left e -> lift $ outputStrLn $ show (e :: IOException)
    Right _ -> pure ()
