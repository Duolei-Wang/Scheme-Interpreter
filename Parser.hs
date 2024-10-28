module Parser where

import Control.Monad.Except (runExceptT)
import Syntax
import Text.Parsec (runParserT)
import Text.Parsec.Token (GenTokenParser (whiteSpace))
import Text.ParserCombinators.Parsec

type SParser = Parser Expr

specialChar :: Parser Char
specialChar = oneOf "!$%&*/:<?^_-=<"

pExpr :: SParser
pExpr =
  pSymbol
    <|> pEllipsis
    <|> pNumber
    <|> pList
    <|> pQuote

intertokenSpace :: Parser String
intertokenSpace = do
  many $ space <|> oneOf "\a\b\f\n\r\t\v"

atomsphere :: Parser String
atomsphere = spaceOrLine <|> comment

spaceOrLine :: Parser String
spaceOrLine = many $ space <|> char '\n'

comment :: Parser String
comment = do
  intertokenSpace
  head <- char ';'
  content <- manyTill anyChar $ char '\n'
  return $ "<comment>: " ++ content

pSymbol :: SParser
pSymbol = do
  intertokenSpace
  first <- letter <|> oneOf "#+-*/=><"
  rest <- many (letter <|> digit <|> specialChar)
  return $ Symbol (first : rest)

pEllipsis :: SParser
pEllipsis = do
  intertokenSpace
  string "..."
  return $ Symbol "..."

pQuote :: SParser
pQuote = do
  intertokenSpace
  _ <- char '`'
  Quote <$> pExpr

plhs :: Parser Char
plhs = char '('

prhs :: Parser Char
prhs = char ')'

pNumber :: SParser
pNumber = do
  content <- many1 digit
  return $ Number $ read content

pList :: SParser
pList = do
  _ <- char '('
  intertokenSpace
  _ <- spaces
  intertokenSpace
  lst <- endBy pExpr intertokenSpace
  _ <- char ')'
  return $ List lst
