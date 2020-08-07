module Main where

import Parser
import Machine
import Compiler
import Interpreter

import System.Environment
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  if not $ null args
     then do
       code <- readFile $ head args
       let tree = parseStr code
           varTable = buildVarTable tree
           instr = compile' tree
           results = execState (exec tree) Map.empty
           machine = initMachine (length $ Map.keys varTable)
           machine' = execute machine instr
       print tree
       putStrLn ""
       print results
       putStrLn ""
       print varTable
       putStrLn ""
       putStrLn . unlines $ map show instr
       print machine'
       else return ()
