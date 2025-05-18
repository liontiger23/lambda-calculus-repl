module LambdaCalculus.Terms
    ( Term (..)
    , Ident
    , render
    , render'
    , renderTokens'
    ) where

type Ident = String

data Term = Var Ident      -- Variable
          | App Term Term  -- Application
          | Abs Ident Term -- Abstraction
    deriving (Show, Eq)

render :: Term -> String
render = render' "λ"

render' :: String -> Term -> String
render' l = concat . renderTokens' l

renderTokens' :: String -> Term -> [String]
renderTokens' _ (Var x) = [x]
renderTokens' l (App m n) = ["("] ++ renderTokens' l m ++ [" "] ++ renderTokens' l n ++ [")"]
renderTokens' l (Abs x m) = [l, x, "."] ++ renderTokens' l m

