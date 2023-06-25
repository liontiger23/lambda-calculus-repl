module LambdaCalculus.Terms
    ( Term (..)
    , Ident
    ) where

type Ident = String

data Term = Var Ident      -- Variable
          | App Term Term  -- Application
          | Abs Ident Term -- Abstraction
    deriving (Eq)

instance Show Term where
  show (Var x) = x
  show (App m n) = "(" ++ show m ++ " " ++  show n ++ ")"
  show (Abs x m) = "λ" ++ x ++ "." ++ show m

