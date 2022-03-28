module Lang where

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol String |
  List [Value] |
  Function [String] Value Bool |
  Builtin String |
  Nil
  deriving (Show, Eq)
