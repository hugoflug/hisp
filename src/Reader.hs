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
-- List operations (cons)
-- Line numbers at errs
-- Be more liberal with accepted symbols
-- Macro without explicit quoting
-- CLI
-- Better REPL (repline?)
-- Eithers instead of err calls?

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
        Function args body -> do
          when (length args /= length tail) $
            err $ "Wrong number of arguments"
          readTail <- traverse (read' globals locals) tail
          let fnLocals = M.fromList $ zip args readTail
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
  Symbol name -> name
  List vals -> "(" <> (printVals " " vals) <> ")"
  Function args val -> printVal $ List [String' "fn", List (Symbol <$> args), val]
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
    "fn" -> Just $
      case values of
        [List args, body] -> do
          symArgs <- for args $ \arg ->
            case arg of
              Symbol name -> pure name
              x -> err $ "Function argument not a symbol: " <> show x
          pure $ Function symArgs body
        [x, _] -> err $ "First argument to fn not a list: " <> show x
        _ -> err "Wrong number of arguments to fn"
    "'" -> Just $
      case values of
        [value] -> quote globals locals value
        _ -> err "Wrong number of arguments to quote"
    "eval" -> Just $
      case values of
        [value] -> do
          readValue <- read' globals locals value
          read' globals locals readValue
        _ -> err "Wrong number of arguments to unquote"
    "cons" -> Just $
      case values of
        [val, listArg] -> do
          readList <- read' globals locals listArg
          case readList of
            List list -> pure $ List $ val : list
            _ -> err "Second argument to cons not a list"
        _ -> err "Wrong number of arguments to cons"
    _ -> Nothing

-- TODO: write in hisp instead?
quote :: IORef (M.Map String Value) -> M.Map String Value -> Value -> IO Value
quote globals locals val =
  case val of
    List [Symbol "~", v] -> read' globals locals v
    List vals -> do
      qVals <- traverse (quote globals locals) vals
      pure $ List qVals
    Function args body -> do
      qBody <- quote globals locals body
      pure $ Function args qBody
    v -> pure v