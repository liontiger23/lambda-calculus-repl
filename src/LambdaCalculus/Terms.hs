module LambdaCalculus.Terms
    ( Term (..)
    , Ident
    , render
    ) where

import Data.Text (Text)

type Ident = Text

data Term = Var Ident      -- Variable
          | App Term Term  -- Application
          | Abs Ident Term -- Abstraction
    deriving (Show, Eq)

render :: Term -> Text
render (Var x) = x
render (App m n) = "(" <> render m <> " " <> render n <> ")"
render (Abs x m) = "λ" <> x <> "." <> render m

