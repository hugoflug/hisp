{-# LANGUAGE TypeApplications #-}

module Main where

import Run
import System.Environment (getArgs)
import Control.Exception (handle, displayException, SomeException, ErrorCall(..))
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    filename:_ -> 
      handle (\(ErrorCall s) -> putStrLn s *> exitFailure) $ do 
        runFile filename
    _ -> repl
