{-# LANGUAGE TypeApplications #-}

module Run where

import Parse 
import Eval
import Lang

import Control.Exception (handle, displayException, SomeException, ErrorCall(..))
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
  absFilename <- makeAbsolute filename
  let currDir = takeDirectory absFilename
  values <- run' absFilename currDir program
  putStrLn $ printVals "\n" values

run' :: String -> String -> String -> IO [Value]
run' filename currDir program = do
  globals <- newIORef M.empty
  run filename currDir globals program

run :: String -> String -> IORef (M.Map SymbolName SymbolValue) -> String -> IO [Value]
run filename currDir globals program = 
  case parse filename program of
    Left err -> error $ show err
    Right exprs -> do
      currNs <- newIORef Nothing
      traverse (eval (Context globals M.empty currDir currNs [])) exprs

repl :: IO ()
repl = do
  globals <- newIORef M.empty
  forever $ do
    putStr "λ> "
    hFlush stdout
    line <- getLine
    workDir <- getCurrentDirectory
    handle (\(ErrorCall s) -> putStrLn s) $ do 
      values <- run "repl" workDir globals line
      putStrLn $ printVals "\n" values
