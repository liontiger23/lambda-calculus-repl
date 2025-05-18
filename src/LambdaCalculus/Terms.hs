module LambdaCalculus.Terms
    ( Term (..)
    , Ident
    , render
    ) where

type Ident = String

data Term = Var Ident      -- Variable
          | App Term Term  -- Application
          | Abs Ident Term -- Abstraction
    deriving (Show, Eq)

render :: Term -> String
render (Var x) = x
render (App m n) = "(" <> render m <> " " <> render n <> ")"
render (Abs x m) = "λ" <> x <> "." <> render m

