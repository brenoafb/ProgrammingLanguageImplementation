module Main where

import Parser
import Machine
import Compiler
import Interpreter

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import System.IO ( isEOF )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> interpreter fileName
    ["-c", fileName] -> compiler fileName
    _ -> usage

interpreter :: FilePath -> IO ()
interpreter fp = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right stmt -> print $ execState (exec stmt) Map.empty

compiler :: FilePath -> IO ()
compiler fp = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right stmt -> do
      let table = buildVarTable stmt
          instr = compile' stmt
          machine = initMachine (length $ Map.keys table)
          machine' = execute machine instr
      printList instr
      putStrLn ""
      print machine'

usage :: IO ()
usage = putStrLn "usage: imperative [-c] <file>"

printList = mapM print
