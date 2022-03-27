module Reader where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Traversable (for)
import Lang

-- Quoting
-- Let (could be a macro?)
-- Closures?
-- Line numbers at errors
-- Be more liberal with accepted symbols

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
            Nothing -> error $ "No such symbol: " <> name
    List (fn : tail) ->
      case fn of
        Function args body -> do
          when (length args /= length tail) $
            error $ "Wrong number of arguments"
          let fnLocals = M.fromList $ zip args tail
          read' globals fnLocals body
        sym@(Symbol name) -> do
          case builtIn globals locals name tail of
            Just action -> action
            Nothing -> do
              readSym <- read' globals locals sym
              read' globals locals (List (readSym : tail))
        _ -> error "No function at head of list"
    v -> pure v

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Nil -> "nil"

builtIn :: IORef (M.Map String Value) -> M.Map String Value -> String -> [Value] -> Maybe (IO Value)
builtIn globals locals name values =
  case name of
    "print" -> Just $ do
      readValues <- traverse (read' globals locals) values
      putStrLn $ intercalate " " $ printVal <$> readValues
      pure Nil
    "+" -> Just $ do
      readValues <- traverse (read' globals locals) values
      intArgs <- for readValues $ \arg ->
            case arg of
              Int' i -> pure i
              _ -> error "Argument to + not an integer"
      pure $ Int' $ sum intArgs
    "def" -> Just $
      case values of
        [Symbol name, val] -> do
          readVal <- read' globals locals val
          modifyIORef globals $ \glob ->
            M.insert name readVal glob
          pure Nil
        [_, _] -> error "First argument to def not a symbol"
        _ -> error "Wrong number of arguments to def"
    "fn" -> Just $
      case values of
        [List args, body] -> do
          symArgs <- for args $ \arg ->
            case arg of
              Symbol name -> pure name
              _ -> error "Function argument not a symbol"
          pure $ Function symArgs body
        [_, _] -> error "First argument to fn not a list"
        _ -> error "Wrong number of arguments to fn"
    _ -> Nothing