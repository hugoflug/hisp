module Lang where

import Text.Parsec (SourcePos)

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol String SourcePos |
  List [Value] SourcePos |
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
  Split |
  Import
  deriving (Show, Eq)