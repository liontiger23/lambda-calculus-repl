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
term = label "Term" $ space *> (abs' <|> app <|> var)

var :: Parser Term
var = label "Var" $ fmap Var ident

app :: Parser Term
app = label "App" $ between (single '(') (space *> single ')') $
    try (App <$> term <*> term) <|>
    try (App <$> var <*> term)


abs' :: Parser Term
abs' = label "Abs" $ Abs <$> between ((chunk "\\" <|> chunk "λ") <* space) (space *> single '.' <* space) ident <*> term

ident :: Parser Ident
ident = label "Ident" $ (:) <$> letterChar <*> many alphaNumChar
