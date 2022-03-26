module Run where

import Parse 
import Reader
import Lang

import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.IORef (newIORef)

run :: String -> IO [Value]
run program = 
  case parse "" program of
    Left err -> error $ show err
    Right exprs -> do
      register <- newIORef M.empty
      traverse (readExpr register) exprs