{-# LANGUAGE TypeApplications #-}

module Run where

import Parse 
import Eval
import Lang

import Control.Exception (handle, displayException, SomeException)
import Control.Monad (forever, void)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.IORef (newIORef, IORef)
import System.IO (hFlush, stdout, getLine)

runFile :: FilePath -> IO ()
runFile filename = do
  program <- readFile filename
  values <- run' program
  putStrLn $ printVals "\n" values

run :: IORef (M.Map String Value) -> String -> IO [Value]
run globals program = 
  case parse "" program of
    Left err -> error $ show err
    Right exprs -> traverse (eval globals M.empty) exprs

run' :: String -> IO [Value]
run' program = do
  globals <- newIORef M.empty
  run globals program

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
