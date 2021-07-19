module Main where

import Parser
import Interpreter
import Compiler
import Machine

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
       let program = parseStr code
           result = exec program
           compiled = compile program
       print result
       case compiled of
         Left err -> print err
         Right asm -> do
           print ""
           printASM asm
           let machine = initMachine 32
               executionResult = execute machine asm
           print ""
           print executionResult
       else return ()
