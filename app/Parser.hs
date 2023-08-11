{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser where
import Types
import Data.Text (Text)
import Text.Megaparsec.Char
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Compiler
type LTLParser = Parsec String String
{-
    The grammar is basically 
    F ::=  []F
          |<>(<=Int)F
          |<>F
          |(F /\ F)
          |(F => F)
          | (F (< | > | >= | <=) F)
          |opName(F?(, F)*)
          |varName
-}
    
parseString :: String -> IO LTLForm
parseString input = 
    case parse parseLTL "(PBLTL formula)" input of
      --These error messages are borderline worthless. I think megaparsec has a way to improve them. TODO.
      Left e -> ioError (userError $ "failed to parse with error: " ++ show e)
      Right f -> pure f

symbol :: String -> LTLParser String
symbol = L.symbol space

parseId :: LTLParser String 
parseId =  L.lexeme space
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: LTLParser a -> LTLParser a
parens = between (L.symbol space "(") (L.symbol space ")")

parseLTL :: LTLParser LTLForm
parseLTL =
    parseBox 
    <|> parseNeg
    <|> try parseBDiamond
    <|> try parseDiamond
    <|> try parseAnd
    <|> try parseBinOp
    <|> try parseImplies
    <|> try parseOp
    <|> parseVar
    <|> parseInt
    <|> parens parseLTL 

parseBox :: LTLParser LTLForm
parseBox = L.symbol space "[]" >> LBox <$> parseLTL

parseNeg :: LTLParser LTLForm
parseNeg = L.symbol space "~" >> LNeg <$> parseLTL

parseDiamond :: LTLParser LTLForm
parseDiamond = do
    L.symbol space "<>"
    f <- parseLTL
    pure $ LDiamond f
parseBDiamond :: LTLParser LTLForm
parseBDiamond = do
    L.symbol space "<>"
    n <- parens (L.symbol space "<=" >> L.decimal)
    f <- parseLTL
    pure $ LBDiamond n f

parseAnd :: LTLParser LTLForm
parseAnd = do
    symbol "("
    l <- parseLTL
    symbol "/\\"
    r <- parseLTL
    symbol ")"
    pure $ (l `LAnd` r)
parseBinOp :: LTLParser LTLForm
parseBinOp = do
    symbol "("
    l <- parseLTL
    op <- choice (map symbol [">","<",">=","<=","="])
    r <- parseLTL
    symbol ")"
    pure $ (LBinOp l op r)

parseImplies :: LTLParser LTLForm
parseImplies = do
    symbol "("
    l <- parseLTL
    symbol "=>"
    r <- parseLTL
    symbol ")"
    pure $ (l `LImplies` r)

parseVar :: LTLParser LTLForm
parseVar = LAtom <$> parseId

parseInt :: LTLParser LTLForm
parseInt = LInt <$> L.decimal

parseOp :: LTLParser LTLForm
parseOp = do
    id <- parseId
    symbol "("
    f <- optional parseLTL
    case f of
      Just e -> do 
          fs <- many (L.symbol space "," >> parseLTL)
          symbol ")" 
          pure $ LOp id (e:fs)
      Nothing -> do 
          symbol ")" 
          pure $ LOp id []

