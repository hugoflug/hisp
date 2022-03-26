module Reader where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lang

readExpr :: Expr -> IO Value
readExpr expr = 
  case expr of
    IntExpr i -> pure $ IntValue i
    StringExpr s -> pure $ StringValue s
    SymbolExpr _ -> pure Nil -- TODO: implement
    ListExpr (fn : tail) ->
      case fn of
        SymbolExpr name -> do
          values <- traverse readExpr tail
          case builtIn name values of
            Just action -> action
            Nothing -> error "No such built-in"
        _ -> error "No symbol in function position"

printVal :: Value -> String 
printVal v = case v of
  IntValue i -> show i
  StringValue i -> show i
  Nil -> "nil"

builtIn :: String -> [Value] -> Maybe (IO Value)
builtIn name values =
  case name of
    "print" -> Just $ do
      putStrLn $ intercalate " " $ printVal <$> values
      pure Nil
    _ -> Nothing