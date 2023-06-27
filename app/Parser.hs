{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser where
import Types
import Data.Text (Text)
import Text.Megaparsec.Char
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
type Parser = Parsec String String
{-
    The grammar is basically 
    F ::=  []F
          |<>(<=Int)F
          |(F /\ F)
          |(F => F)
          |opName(F?(, F)*)
          |varName
-}

symbol :: String -> Parser String
symbol = L.symbol space

parseId :: Parser String 
parseId =  L.lexeme space
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (L.symbol space "(") (L.symbol space ")")

parseLTL :: Parser LTLForm
parseLTL =
    parseBox 
    <|> parseDiamond
    <|> parseNeg
    <|> try parseAnd
    <|> try parseImplies
    <|> try parseOp
    <|> parseVar

parseBox :: Parser LTLForm
parseBox = L.symbol space "[]" >> LBox <$> parseLTL

parseNeg :: Parser LTLForm
parseNeg = L.symbol space "~" >> LNeg <$> parseLTL

parseDiamond :: Parser LTLForm
parseDiamond = do
    L.symbol space "<>"
    n <- parens (L.symbol space "<=" >> L.decimal)
    f <- parseLTL
    pure $ LDiamond n f

parseAnd :: Parser LTLForm
parseAnd = do
    symbol "("
    l <- parseLTL
    symbol "/\\"
    r <- parseLTL
    symbol ")"
    pure $ (l `LAnd` r)

parseImplies :: Parser LTLForm
parseImplies = do
    symbol "("
    l <- parseLTL
    symbol "=>"
    r <- parseLTL
    symbol ")"
    pure $ (l `LImplies` r)

parseVar :: Parser LTLForm
parseVar = LAtom <$> parseId

parseOp :: Parser LTLForm
parseOp = do
    id <- parseId
    symbol "("
    f <- optional parseLTL
    case f of
      Just e -> do 
          fs <- many (L.symbol space "," >> parseLTL)
          symbol "(" 
          pure $ LOp id (e:fs)
      Nothing -> do 
          symbol ")" 
          pure $ LOp id []
