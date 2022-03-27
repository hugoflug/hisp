module Reader where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Traversable (for)
import Lang

-- Let (could be a macro?)
-- Closures?
-- Rename 'read' to 'eval'
-- Line numbers at errs
-- Be more liberal with accepted symbols
-- CLI
-- Better REPL (repline?)
-- Remove Function constructor?
-- Improve exceptions
-- Refactor
-- Automatically load core.hisp

read' :: IORef (M.Map String Value) -> M.Map String Value -> Value -> IO Value
read' globals locals val = 
  case val of
    Symbol name -> do
      case locals !? name of
        Just v -> pure v
        Nothing -> do
          reg <- readIORef globals
          case reg !? name of
            Just v -> pure v
            Nothing -> err $ "No such symbol: " <> name
    List (fn : tail) ->
      case fn of
        Function args body macro -> do
          when (length args /= length tail) $
            err $ "Wrong number of arguments"
          fnTail <- if macro then
              pure tail
            else
              traverse (read' globals locals) tail
          let fnLocals = M.fromList $ zip args fnTail
          read' globals fnLocals body
        sym@(Symbol name) -> do
          case builtIn globals locals name tail of
            Just action -> action
            Nothing -> do
              readSym <- read' globals locals sym
              read' globals locals (List (readSym : tail))
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
  Function args val macro -> printVal $ List [String' (if macro then "macro" else "fn"), List (Symbol <$> args), val]
  Nil -> "nil"

printVals :: String -> [Value] -> String
printVals separator = intercalate separator . fmap printVal

err :: String -> IO a
err = ioError . userError

builtIn :: IORef (M.Map String Value) -> M.Map String Value -> String -> [Value] -> Maybe (IO Value)
builtIn globals locals name values =
  case name of
    "print" -> Just $ do
      readValues <- traverse (read' globals locals) values
      putStrLn $ printVals " " readValues
      pure Nil
    "+" -> Just $ do
      readValues <- traverse (read' globals locals) values
      intArgs <- for readValues $ \arg ->
            case arg of
              Int' i -> pure i
              _ -> err "Argument to + not an integer"
      pure $ Int' $ sum intArgs
    "def" -> Just $
      case values of
        [Symbol name, val] -> do
          readVal <- read' globals locals val
          modifyIORef globals $ \glob ->
            M.insert name readVal glob
          pure Nil
        [_, _] -> err "First argument to def not a symbol"
        _ -> err "Wrong number of arguments to def"
    "fn" -> Just $ fn globals locals False values
    "macro" -> Just $ fn globals locals True values
    "'" -> Just $
      case values of
        [value] -> quote globals locals value
        _ -> err "Wrong number of arguments to '"
    "eval" -> Just $
      case values of
        [value] -> do
          readValue <- read' globals locals value
          read' globals locals readValue
        _ -> err "Wrong number of arguments to eval"
    "cons" -> Just $
      case values of
        [val, listArg] -> do
          readList <- read' globals locals listArg
          case readList of
            List list -> pure $ List $ val : list
            _ -> err "Second argument to cons not a list"
        _ -> err "Wrong number of arguments to cons"
    "head" -> Just $
      case values of
        [val] -> do
          readList <- read' globals locals val
          case readList of
            List (head : _) -> read' globals locals head
            _ -> err "First argument to head not a list"
        _ -> err "Wrong number of arguments to head"
    "tail" -> Just $
      case values of
        [val] -> do
          readList <- read' globals locals val
          case readList of
            List (_ : tail) -> do
              readTail <- traverse (read' globals locals) tail
              pure $ List readTail
            _ -> err "First argument to tail not a list"
        _ -> err "Wrong number of arguments to tail"
    "if" -> Just $
      case values of
        [condition, then', else'] -> do
          readCondition <- read' globals locals condition
          case readCondition of
            Bool' False -> read' globals locals else'
            Nil -> read' globals locals else'
            _ -> read' globals locals then'
        _ -> err "Wrong number of arguments to if"
    _ -> Nothing

fn :: IORef (M.Map String Value) -> M.Map String Value -> Bool -> [Value] -> IO Value
fn globals locals macro values =
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
    List [Symbol "~", v] -> read' globals locals v
    List vals -> do
      qVals <- traverse (quote globals locals) vals
      pure $ List qVals
    Function args body macro -> do
      qBody <- quote globals locals body
      pure $ Function args qBody macro
    v -> pure v