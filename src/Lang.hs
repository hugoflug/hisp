module Lang where

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol String |
  List [Value] |
  Function [String] Value Bool |
  Nil
  deriving (Show, Eq)
