module Reader where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?))
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Lang

readExpr :: IORef (M.Map String Value) -> Expr -> IO Value
readExpr register expr = 
  case expr of
    IntExpr i -> pure $ IntValue i
    StringExpr s -> pure $ StringValue s
    SymbolExpr name -> do
      reg <- readIORef register
      case reg !? name of
        Just val -> pure val
        Nothing -> pure Nil -- or fail?
    ListExpr (fn : tail) ->
      case fn of
        SymbolExpr name -> do
          case builtIn register name tail of
            Just action -> action
            Nothing -> error "No such built-in"
        _ -> error "No symbol in function position"

printVal :: Value -> String 
printVal v = case v of
  IntValue i -> show i
  StringValue i -> show i
  Nil -> "nil"

builtIn :: IORef (M.Map String Value) -> String -> [Expr] -> Maybe (IO Value)
builtIn register name exprs =
  case name of
    "print" -> Just $ do
      values <- traverse (readExpr register) exprs
      putStrLn $ intercalate " " $ printVal <$> values
      pure Nil
    "def" -> Just $
      case exprs of
        [SymbolExpr name, valExpr] -> do
          val <- readExpr register valExpr
          modifyIORef register $ \reg ->
            M.insert name val reg
          pure Nil
        [_, _] -> error "First argument to def not a symbol"
        _ -> error "Wrong number of arguments to def"
    _ -> Nothing