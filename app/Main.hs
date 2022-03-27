module Main where

import Run
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    filename:_ -> runFile filename
    _ -> repl
