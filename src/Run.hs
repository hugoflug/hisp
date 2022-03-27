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
      globals <- newIORef M.empty
      let locals = M.empty
      traverse (read' globals locals) exprs