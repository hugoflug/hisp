module Eval where

import Control.Monad (when, join)
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

data Context = Context {
  globals :: IORef (M.Map String Value),
  locals :: M.Map String Value
}

eval :: Context -> Value -> IO Value
eval ctx@(Context globals locals) val =
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
      evaledHead <- eval ctx head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin ctx name tail
        Function args body macro -> do
          when (length args /= length tail) $
            err $ "Wrong number of arguments"
          fnTail <- if macro then
              pure tail
            else
              traverse (eval ctx) tail
          let fnLocals = M.fromList $ zip args fnTail
          eval ctx{locals = fnLocals} body
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
    "=" -> Just Equals
    "join" -> Just Join
    "split" -> Just Split
    _ -> Nothing

evalBuiltin :: Context -> Builtin -> [Value] -> IO Value
evalBuiltin ctx@(Context globals locals) builtin values =
  case builtin of
    Print -> do
      evaledValues <- traverse (eval ctx) values
      putStrLn $ printVals " " evaledValues
      pure Nil
    Plus -> do
      evaledValues <- traverse (eval ctx) values
      intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Int' i -> pure i
              _ -> typeErr ix "+" "integer"
      pure $ Int' $ sum intArgs
    Def -> 
      case values of
        [Symbol name, val] -> do
          evaledVal <- eval ctx val
          modifyIORef globals $ \glob ->
            M.insert name evaledVal glob
          pure Nil
        [_, _] -> typeErr 1 "def" "symbol"
        _ -> arityErr "def"
    Fn -> evalFn False values
    Macro -> evalFn True values
    Quote -> 
      case values of
        [value] -> quote ctx value
        _ -> arityErr "'"
    Eval ->
      case values of
        [value] -> do
          evaledValue <- eval ctx value
          eval ctx evaledValue
        _ -> arityErr "eval"
    Cons ->
      case values of
        [val, listArg] -> do
          evaledVal <- eval ctx val
          evaledList <- eval ctx listArg
          case evaledList of
            List list -> pure $ List $ evaledVal : list
            _ -> typeErr 2 "cons" "list"
        _ -> arityErr "cons"
    Head ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (head : _) -> pure head
            _ -> typeErr 1 "head" "list"
        _ -> arityErr "head"
    Tail ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (_ : tail) -> pure $ List tail
            _ -> typeErr 1 "tail" "list"
        _ -> arityErr "tail"
    If ->
      case values of
        [condition, then', else'] -> do
          evaledCondition <- eval ctx condition
          case evaledCondition of
            Bool' False -> eval ctx else'
            Nil -> eval ctx else'
            _ -> eval ctx then'
        _ -> arityErr "if"
    Equals -> 
      case values of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          pure $ Bool' $ evaledLhs == evaledRhs
        _ -> arityErr "="
    Join ->
      case values of
        [List list] -> do
          stringArgs <- for list $ \arg ->
            case arg of
              String' s -> pure s
              v -> err $ "List in argument to join contained: " <> printVal v <> ". Only strings are allowed"
          pure $ String' $ join stringArgs
        [_] -> typeErr 1 "join" "list"
        _ -> arityErr "join"
    Split ->
      case values of
        [String' s] -> do
          pure $ List $ (\c -> String' [c]) <$> s
        [_] -> typeErr 1 "split" "string"
        _ -> arityErr "split"

typeErr :: Int -> String -> String -> IO a
typeErr argNo fnName typeName = err $ "Argument " <> show argNo <> " to " <> fnName <> " not of type " <> typeName

arityErr :: String -> IO a
arityErr fnName = err $ "Wrong number of arguments to " <> fnName

evalFn :: Bool -> [Value] -> IO Value
evalFn macro values =
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
quote :: Context -> Value -> IO Value
quote ctx@(Context globals locals) val =
  case val of
    List [Symbol "~", v] -> eval ctx v
    List vals -> do
      qVals <- traverse (quote ctx) vals
      pure $ List qVals
    Function args body macro -> do
      qBody <- quote ctx body
      pure $ Function args qBody macro
    v -> pure v