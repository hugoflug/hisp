module Parse where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Lang (Value (..), SymbolName(..))
import Data.List.Split (splitOn)

import Data.Char (isAlpha, isAlphaNum, isAscii, isDigit)

def =
  emptyDef
    {
      commentLine = ";"
    , identStart = noneOf "()\"; \t\n12334567890"
    , identLetter = noneOf "()\"; \t\n"
    }

TokenParser 
  {
    identifier = m_identifier
  , integer = m_integer
  , stringLiteral = m_stringLit
  , whiteSpace = m_whiteSpace
  , symbol = m_symbol
  } = makeTokenParser def

expr = list <|> symbolExpr <|> int <|> strExpr

list = do
  pos <- getPosition
  m_symbol "("
  exprs <- many expr
  m_symbol ")"
  pure $ List exprs pos

int = Int' <$> m_integer

strExpr = String' <$> m_stringLit

symbolExpr = do
  pos <- getPosition
  sym <- m_identifier
  let parts = splitOn "/" sym

  symName <- case parts of
    [name] -> pure $ SymbolName Nothing name
    ["", ""] -> pure $ SymbolName Nothing "/"
    ["", _] -> fail $ "/ not allowed at beginning of symbol"
    [_, ""] -> fail $ "/ not allowed at end of symbol"
    [namespace, name] -> pure $ SymbolName (Just namespace) name
    p -> fail $ "/ allowed only once in a symbol"
  pure $ Symbol symName pos

program = do
  m_whiteSpace
  exprs <- many expr
  eof
  pure exprs

parse :: SourceName -> String -> Either ParseError [Value]
parse = Text.Parsec.parse program