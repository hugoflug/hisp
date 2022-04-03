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
import System.FilePath (takeDirectory)
import System.Directory (makeAbsolute, getCurrentDirectory)

runFile :: FilePath -> IO ()
runFile filename = do
  program <- readFile filename
  currDir <- makeAbsolute $ takeDirectory filename
  values <- run' currDir program
  putStrLn $ printVals "\n" values

run' :: String -> String -> IO [Value]
run' currDir program = do
  globals <- newIORef M.empty
  run currDir globals program

run :: String -> IORef (M.Map String Value) -> String -> IO [Value]
run currDir globals program = 
  case parse "" program of
    Left err -> error $ show err
    Right exprs -> traverse (eval (Context globals M.empty currDir)) exprs

repl :: IO ()
repl = do
  globals <- newIORef M.empty
  forever $ do
    putStr "λ> "
    hFlush stdout
    line <- getLine
    workDir <- getCurrentDirectory
    handle @SomeException (putStrLn . show) $ do 
      values <- run workDir globals line
      putStrLn $ printVals "\n" values
