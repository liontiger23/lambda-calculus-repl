module LambdaCalculus.Parser
    ( term
    , parseTerm
    ) where

import LambdaCalculus.Terms
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseTerm :: Text -> Either Text Term
parseTerm = left (T.pack . errorBundlePretty) . runParser (term <* eof) ""

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
ident = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
