module Lang where

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol String |
  List [Value] |
  Function [String] Value Bool |
  Builtin' Builtin |
  Nil
  deriving (Show, Eq)

data Builtin =
  Print |
  Plus |
  Def |
  Fn |
  Macro |
  Quote |
  Eval |
  Cons |
  Head |
  Tail |
  If |
  Equals |
  Join |
  Split
  deriving (Show, Eq)