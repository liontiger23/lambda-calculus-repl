module LambdaCalculus.Parser
    ( term
    , parseTerm
    , parseTermDef
    ) where

import LambdaCalculus.Terms
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Monoid (Endo(..))

type Parser = Parsec Void String

parseTerm :: String -> Either String Term
parseTerm = left errorBundlePretty . runParser (term <* eof) ""

parseTermDef :: String -> Either String (Ident, Term)
parseTermDef = left errorBundlePretty . runParser (def <* eof) ""

def :: Parser (Ident, Term)
def = (,) <$> ident <* lexeme (single '=') <*> term

left :: (a -> b) -> Either a c -> Either b c
left f (Left a)  = Left (f a)
left _ (Right c) = Right c

term :: Parser Term
term = label "Term" $ space *> (abs' <|> app <|> var)

var :: Parser Term
var = label "Var" $ lexeme $ fmap Var ident

varName :: Parser VarName
varName = label "VarName" $ lexeme $ (:) <$> letterChar <*> many identChar

appTerm :: Parser Term
appTerm = choice [abs', parens app, var]

app :: Parser Term
app = makeExprParser appTerm
  [ [ InfixL (App <$ chunk "") ]
  ]

abs' :: Parser Term
abs' = label "Abs" $ lexeme $
  between (lexeme (chunk "\\" <|> chunk lambda)) (lexeme $ single '.')
    -- TODO: write tests for \x y z.x cases
    (appEndo . mconcat . fmap Endo <$> some (Abs <$> lexeme varName))
  <*> term

ident :: Parser Ident
ident = label "Ident" $ lexeme $ (:) <$> alphaNumChar <*> many identChar

identChar :: Parser Char
identChar = alphaNumChar <|> single '\''

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parens :: Parser a -> Parser a
parens = between (lexeme $ single '(') (lexeme $ single ')')
