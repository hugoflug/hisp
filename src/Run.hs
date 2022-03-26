module Run where

import Parse 
import Reader

import Data.Foldable (traverse_)

run :: String -> IO ()
run program = 
  case parse "" program of
    Left err -> print err
    Right exprs -> traverse_ readExpr exprs