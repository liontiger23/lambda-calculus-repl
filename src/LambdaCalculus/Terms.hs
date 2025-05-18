module LambdaCalculus.Terms
    ( Term (..)
    , Ident
    , lambda
    , render
    , render'
    , renderTokens'
    , reduce
    , reduceFully
    , substitute
    ) where
import Control.Applicative ((<|>))

type Ident = String

data Term = Var Ident      -- Variable
          | App Term Term  -- Application
          | Abs Ident Term -- Abstraction
    deriving (Show, Eq)

render :: Term -> String
render = render' lambda

render' :: String -> Term -> String
render' l = concat . renderTokens' l

renderTokens' :: String -> Term -> [String]
renderTokens' _ (Var x) = [x]
renderTokens' l (App m n) = ["("] ++ renderTokens' l m ++ [" "] ++ renderTokens' l n ++ [")"]
renderTokens' l (Abs x m) = [l, x, "."] ++ renderTokens' l m

reduceFully :: Term -> [Term]
reduceFully t = case reduce t of
  Nothing -> [t]
  Just t' -> t : reduceFully t'

reduce :: Term -> Maybe Term
reduce (Var _) = Nothing
reduce (Abs x m) = Abs x <$> reduce m
reduce (App (Abs x m) n) = Just $ substitute x n m
reduce (App m n) = (App <$> reduce m <*> pure n) <|> (App m <$> reduce n)

substitute :: Ident -> Term -> Term -> Term
substitute x s (Var y) | x == y    = s
                       | otherwise = Var y
substitute x s (App m n) = App (substitute x s m) (substitute x s n)
-- Note: when starting from closed terms (with all variables bound)
-- we don't need to do any renaming
substitute x s (Abs y m) = Abs y (substitute x s m)

lambda :: String
lambda = "λ"
