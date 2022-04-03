module Lang where

import Text.Parsec (SourcePos)
import qualified Data.Map as M

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol String SourcePos |
  List [Value] SourcePos |
  Function {
    args :: [String],
    body :: Value,
    captures :: M.Map String Value,
    isMacro :: Bool
  } |
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
  Gt |
  Join |
  Split |
  Import
  deriving (Show, Eq)