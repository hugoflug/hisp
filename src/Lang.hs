module Lang where

data Value = 
  Int' Integer |
  String' String |
  Symbol String |
  List [Value] |
  Nil
  deriving (Show, Eq)
