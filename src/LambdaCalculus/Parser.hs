{-# LANGUAGE FlexibleContexts #-}

module LambdaCalculus.Parser
    ( term
    ) where

import LambdaCalculus.Terms
import Text.Parsec

term :: (Stream s m Char) => ParsecT s u m Term
term = spaces *> (var <|> app <|> abs')

var :: (Stream s m Char) => ParsecT s u m Term
var = fmap Var ident

app :: (Stream s m Char) => ParsecT s u m Term
app = between (char '(') (char ')') $
    try (App <$> term <*> term) <|>
    try (App <$> var <*> term)


abs' :: (Stream s m Char) => ParsecT s u m Term
abs' = Abs <$> between (char '\\' <* spaces) (spaces *> char '.' <* spaces) ident <*> term

ident :: (Stream s m Char) => ParsecT s u m Ident
ident = (:) <$> letter <*> many alphaNum
