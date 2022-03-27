{-# LANGUAGE TypeApplications #-}

module Run where

import Parse 
import Reader
import Lang

import Control.Exception (handle, displayException, SomeException)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.IORef (newIORef, IORef)
import System.IO (hFlush, stdout, getLine)

run :: IORef (M.Map String Value) -> String -> IO [Value]
run globals program = 
  case parse "" program of
    Left err -> error $ show err
    Right exprs -> traverse (read' globals M.empty) exprs

repl :: IO ()
repl = do
  globals <- newIORef M.empty
  forever $ do
    putStr "λ> "
    hFlush stdout
    line <- getLine
    handle @SomeException (putStrLn . show) $ do 
      values <- run globals line
      putStrLn $ printVals "\n" values
