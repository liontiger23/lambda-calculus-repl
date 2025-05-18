module LambdaCalculus.Parser
    ( term
    , parseTerm
    ) where

import LambdaCalculus.Terms
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

parseTerm :: String -> Either String Term
parseTerm = left errorBundlePretty . runParser (term <* eof) ""

left :: (a -> b) -> Either a c -> Either b c
left f (Left a)  = Left (f a)
left _ (Right c) = Right c

term :: Parser Term
term = label "Term" $ space *> (abs' <|> app <|> var)

var :: Parser Term
var = label "Var" $ lexeme $ fmap Var ident

appTerm :: Parser Term
appTerm = choice [abs', parens app, var]

app :: Parser Term
app = makeExprParser appTerm
  [ [ InfixL (App <$ chunk "") ]
  ]

abs' :: Parser Term
abs' = label "Abs" $ lexeme $ Abs <$> between (lexeme (chunk "\\" <|> chunk lambda)) (lexeme $ single '.') ident <*> term

ident :: Parser Ident
ident = label "Ident" $ lexeme $ (:) <$> letterChar <*> many alphaNumChar

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parens :: Parser a -> Parser a
parens = between (lexeme $ single '(') (lexeme $ single ')')
