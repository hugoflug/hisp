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
          values <- traverse (readExpr register) tail
          case builtIn register name values of
            Just action -> action
            Nothing -> error "No such built-in"
        _ -> error "No symbol in function position"

printVal :: Value -> String 
printVal v = case v of
  IntValue i -> show i
  StringValue i -> show i
  Nil -> "nil"

builtIn :: IORef (M.Map String Value) -> String -> [Value] -> Maybe (IO Value)
builtIn register name values =
  case name of
    "print" -> Just $ do
      putStrLn $ intercalate " " $ printVal <$> values
      pure Nil
    "def" -> Just $
      case values of
        [StringValue name, val] -> do -- TODO: should be symbol instead of string
          modifyIORef register $ \reg ->
            M.insert name val reg
          pure Nil
        _ -> error "Wrong arguments to def"

    _ -> Nothing