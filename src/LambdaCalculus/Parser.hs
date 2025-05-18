module LambdaCalculus.Parser
    ( term
    , parseTerm
    ) where

import LambdaCalculus.Terms
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char

type Parser = Parsec Void String

parseTerm :: String -> Either String Term
parseTerm = left errorBundlePretty . runParser (term <* eof) ""

left :: (a -> b) -> Either a c -> Either b c
left f (Left a)  = Left (f a)
left _ (Right c) = Right c

term :: Parser Term
term = space *> (var <|> app <|> abs')

var :: Parser Term
var = fmap Var ident

app :: Parser Term
app = between (single '(') (single ')') $
    try (App <$> term <*> term) <|>
    try (App <$> var <*> term)


abs' :: Parser Term
abs' = Abs <$> between (oneOf ['\\', 'λ'] <* space) (space *> single '.' <* space) ident <*> term

ident :: Parser Ident
ident = (:) <$> letterChar <*> many alphaNumChar
