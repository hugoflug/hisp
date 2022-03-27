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
      commentLine = "#"
    , caseSensitive = True
    , identStart = asciiLetter <|> char '-' <|> char '+'
    , identLetter = asciiAlphaNum <|> char '-' <|> char '+'
    , opStart = oneOf []
    , opLetter = oneOf []
    }

TokenParser 
  {
    identifier = m_identifier
  , integer = m_integer
  , stringLiteral = m_stringLit
  , whiteSpace = m_whiteSpace
  , symbol = m_symbol
  } = makeTokenParser def

expr = list <|> int <|> strExpr <|> symbolExpr

list = do
  m_symbol "("
  exprs <- many expr
  m_symbol ")"
  pure $ List exprs

int = Int' <$> m_integer

strExpr = String' <$> m_stringLit

symbolExpr = Symbol <$> m_identifier

program = do
  m_whiteSpace
  exprs <- many expr
  eof
  pure exprs

parse :: SourceName -> String -> Either ParseError [Value]
parse = Text.Parsec.parse program