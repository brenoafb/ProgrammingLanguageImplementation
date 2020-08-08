module Main where

import Parser
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
       let program = parseStr code
           result = exec program
       print result
       else return ()
