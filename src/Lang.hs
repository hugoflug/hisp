module Lang where

import Text.Parsec (SourcePos)
import qualified Data.Map as M

data Value = 
  Bool' Bool |
  Int' Integer |
  String' String |
  Symbol SymbolName SourcePos |
  List [Value] SourcePos |
  Function {
    formals :: [Formal],
    variadicFormal :: Maybe String,
    body :: Value,
    captures :: M.Map String Value,
    isMacro :: Bool
  } |
  Builtin' Builtin |
  Custom String Value |
  Nil
  deriving (Show, Eq)

data SymbolName = SymbolName {
  namespace :: Maybe String,
  localName :: String
} deriving (Eq, Ord)

instance Show SymbolName where
  show (SymbolName namespace localName) = 
    case namespace of
      Just ns -> ns <> "/" <> localName
      Nothing -> localName

data Formal =
  SingleFormal String |
  DestructuringFormal [String]
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
  SetMacro |
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
  Error |
  Type |
  New |
  Unwrap |
  Ns |
  NewSymbol |
  SymbolNameCmd |
  Read
  deriving (Show, Eq)