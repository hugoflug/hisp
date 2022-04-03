module Parse where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Lang (Value (..))

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)

asciiAlphaNum = satisfy (\c -> isAlphaNum c && isAscii c)

asciiLetter = satisfy (\c -> isAlpha c && isAscii c)

def =
  emptyDef
    {
      commentLine = ";"
    , caseSensitive = True
    , identStart = asciiLetter <|> char '-' <|> char '+' <|> char '~' <|> char '\'' <|> char '=' <|> char '>'
    , identLetter = asciiAlphaNum <|> char '-' <|> char '+' <|> char '~' <|> char '\'' <|> char '=' <|> char '>'
    , opStart = oneOf []
    , opLetter = oneOf []
    , reservedNames = ["true", "false", "nil"]
    }

TokenParser 
  {
    identifier = m_identifier
  , integer = m_integer
  , stringLiteral = m_stringLit
  , whiteSpace = m_whiteSpace
  , symbol = m_symbol
  , reserved = m_reserved
  } = makeTokenParser def

expr = list <|> nil <|> boolExpr <|> symbolExpr <|> int <|> strExpr

list = do
  pos <- getPosition
  m_symbol "("
  exprs <- many expr
  m_symbol ")"
  pure $ List exprs pos

int = Int' <$> m_integer

strExpr = String' <$> m_stringLit

boolExpr = trueExpr <|> falseExpr

trueExpr = do
  m_reserved "true"
  pure $ Bool' True

falseExpr = do
  m_reserved "false"
  pure $ Bool' False

nil = do
  m_reserved "nil"
  pure Nil

symbolExpr = do
  pos <- getPosition
  sym <- m_identifier
  pure $ Symbol sym pos

program = do
  m_whiteSpace
  exprs <- many expr
  eof
  pure exprs

parse :: SourceName -> String -> Either ParseError [Value]
parse = Text.Parsec.parse program