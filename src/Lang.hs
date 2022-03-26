module Lang where

data Value = 
  IntValue Integer |
  StringValue String |
  Nil
  deriving (Show, Eq)

data Expr =
  IntExpr Integer |
  StringExpr String |
  SymbolExpr String |
  ListExpr [Expr]
  deriving (Show, Eq)