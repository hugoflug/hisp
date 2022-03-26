module Reader where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Lang

read' :: IORef (M.Map String Value) -> Value -> IO Value
read' register val = 
  case val of
    Symbol name -> do
      reg <- readIORef register
      case reg !? name of
        Just v -> pure v
        Nothing -> error $ "No such symbol: " <> name
    List (fn : tail) ->
      case fn of
        Symbol name -> do
          case builtIn register name tail of
            Just action -> action
            Nothing -> error "No such built-in"
        _ -> error "No function at head of list"
    v -> pure v

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Nil -> "nil"

builtIn :: IORef (M.Map String Value) -> String -> [Value] -> Maybe (IO Value)
builtIn register name values =
  case name of
    "print" -> Just $ do
      readValues <- traverse (read' register) values
      putStrLn $ intercalate " " $ printVal <$> readValues
      pure Nil
    "def" -> Just $
      case values of
        [Symbol name, val] -> do
          readVal <- read' register val
          modifyIORef register $ \reg ->
            M.insert name val reg
          pure Nil
        [_, _] -> error "First argument to def not a symbol"
        _ -> error "Wrong number of arguments to def"
    _ -> Nothing