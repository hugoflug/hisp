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
    varArg :: Maybe String,
    body :: Value,
    captures :: M.Map String Value,
    isMacro :: Bool
  } |
  Builtin' Builtin |
  Nil
  deriving (Show, Eq)

data Builtin =
  Print |
  Readfile |
  Plus |
  Mult |
  Minus |
  Nand |
  Def |
  Fn |
  Macro |
  Quote |
  Eval |
  Apply |
  Cons |
  Head |
  Tail |
  If |
  Equals |
  Gt |
  Split |
  Str | 
  Import |
  Error
  deriving (Show, Eq)