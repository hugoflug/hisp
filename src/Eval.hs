module Eval where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Traversable (for)
import Lang

-- Closures?
-- Line numbers at errs
-- Be more liberal with accepted symbols
-- CLI
-- Better REPL (repline?)
-- Remove Function constructor?
-- Improve exceptions
-- Refactor
-- Automatically load core.hisp
-- It should be possible to override built-ins
-- Built-ins can not be referred to without calling them
-- Think about macroexpand
-- Could fn be a macro that just evals args?

eval :: IORef (M.Map String Value) -> M.Map String Value -> Value -> IO Value
eval globals locals val = 
  case val of
    Symbol name -> do
      case locals !? name of
        Just v -> pure v
        Nothing -> do
          reg <- readIORef globals
          case reg !? name of
            Just v -> pure v
            Nothing -> case parseBuiltin name of
              Just builtin -> pure $ Builtin' builtin
              Nothing -> err $ "No such symbol: " <> name

    List (head : tail) -> do
      evaledHead <- eval globals locals head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin globals locals name tail
        Function args body macro -> do
          when (length args /= length tail) $
            err $ "Wrong number of arguments"
          fnTail <- if macro then
              pure tail
            else
              traverse (eval globals locals) tail
          let fnLocals = M.fromList $ zip args fnTail
          eval globals fnLocals body
        _ -> err "No function at head of list"

    v -> pure v

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Bool' True -> "true"
  Bool' False -> "false"
  Symbol name -> name
  List vals -> "(" <> (printVals " " vals) <> ")"
  Function args val macro -> printVal $ List [Symbol (if macro then "macro" else "fn"), List (Symbol <$> args), val]
  Nil -> "nil"

printVals :: String -> [Value] -> String
printVals separator = intercalate separator . fmap printVal

err :: String -> IO a
err = ioError . userError

parseBuiltin :: String -> Maybe Builtin
parseBuiltin name =
  case name of
    "print" -> Just Print
    "+" -> Just Plus
    "def" -> Just Def
    "fn" -> Just Fn
    "macro" -> Just Macro
    "'" -> Just Quote
    "eval" -> Just Eval
    "cons" -> Just Cons
    "head" -> Just Head
    "tail" -> Just Tail
    "if" -> Just If
    "equals" -> Just Equals
    _ -> Nothing

evalBuiltin :: IORef (M.Map String Value) -> M.Map String Value -> Builtin -> [Value] -> IO Value
evalBuiltin globals locals builtin values =
  case builtin of
    Print -> do
      evaledValues <- traverse (eval globals locals) values
      putStrLn $ printVals " " evaledValues
      pure Nil
    Plus -> do
      evaledValues <- traverse (eval globals locals) values
      intArgs <- for evaledValues $ \arg ->
            case arg of
              Int' i -> pure i
              _ -> err "Argument to + not an integer"
      pure $ Int' $ sum intArgs
    Def -> 
      case values of
        [Symbol name, val] -> do
          evaledVal <- eval globals locals val
          modifyIORef globals $ \glob ->
            M.insert name evaledVal glob
          pure Nil
        [_, _] -> err "First argument to def not a symbol"
        _ -> err "Wrong number of arguments to def"
    Fn -> evalFn globals locals False values
    Macro -> evalFn globals locals True values
    Quote -> 
      case values of
        [value] -> quote globals locals value
        _ -> err "Wrong number of arguments to '"
    Eval ->
      case values of
        [value] -> do
          evaledValue <- eval globals locals value
          eval globals locals evaledValue
        _ -> err "Wrong number of arguments to eval"
    Cons ->
      case values of
        [val, listArg] -> do
          evaledVal <- eval globals locals val
          evaledList <- eval globals locals listArg
          case evaledList of
            List list -> pure $ List $ evaledVal : list
            _ -> err "Second argument to cons not a list"
        _ -> err "Wrong number of arguments to cons"
    Head ->
      case values of
        [val] -> do
          evaledList <- eval globals locals val
          case evaledList of
            List (head : _) -> pure head
            _ -> err "First argument to head not a list"
        _ -> err "Wrong number of arguments to head"
    Tail ->
      case values of
        [val] -> do
          evaledList <- eval globals locals val
          case evaledList of
            List (_ : tail) -> pure $ List tail
            _ -> err "First argument to tail not a list"
        _ -> err "Wrong number of arguments to tail"
    If ->
      case values of
        [condition, then', else'] -> do
          evaledCondition <- eval globals locals condition
          case evaledCondition of
            Bool' False -> eval globals locals else'
            Nil -> eval globals locals else'
            _ -> eval globals locals then'
        _ -> err "Wrong number of arguments to if"
    Equals -> 
      case values of
        [lhs, rhs] -> do
          evaledLhs <- eval globals locals lhs
          evaledRhs <- eval globals locals rhs
          pure $ Bool' $ evaledLhs == evaledRhs
        _ -> err "Wrong number of arguments to ="

evalFn :: IORef (M.Map String Value) -> M.Map String Value -> Bool -> [Value] -> IO Value
evalFn globals locals macro values =
  case values of
    [List args, body] -> do
      symArgs <- for args $ \arg ->
        case arg of
          Symbol name -> pure name
          x -> err $ "Function argument not a symbol: " <> show x
      pure $ Function symArgs body macro
    [x, _] -> err $ "First argument to fn not a list: " <> show x
    _ -> err "Wrong number of arguments to fn"

-- TODO: write in hisp instead?
quote :: IORef (M.Map String Value) -> M.Map String Value -> Value -> IO Value
quote globals locals val =
  case val of
    List [Symbol "~", v] -> eval globals locals v
    List vals -> do
      qVals <- traverse (quote globals locals) vals
      pure $ List qVals
    Function args body macro -> do
      qBody <- quote globals locals body
      pure $ Function args qBody macro
    v -> pure v