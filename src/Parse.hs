module Parse where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Lang (Expr (..))

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)

asciiAlphaNum = satisfy (\c -> isAlphaNum c && isAscii c)

asciiLetter = satisfy (\c -> isAlpha c && isAscii c)

def =
  emptyDef
    {
      commentLine = "#"
    , caseSensitive = True
    , identStart = asciiLetter <|> char '-'
    , identLetter = asciiAlphaNum
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
  pure $ ListExpr exprs

int = IntExpr <$> m_integer

strExpr = StringExpr <$> m_stringLit

symbolExpr = SymbolExpr <$> m_identifier

program = do
  m_whiteSpace
  exprs <- many expr
  eof
  pure exprs

parse :: SourceName -> String -> Either ParseError [Expr]
parse = Text.Parsec.parse program