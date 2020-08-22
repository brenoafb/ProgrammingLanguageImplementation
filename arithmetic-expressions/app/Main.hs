module Main where

import Parser
import Interpreter

import System.IO ( isEOF )

repl :: IO ()
repl = do
  end <- isEOF
  if end
     then return ()
     else do
       code <- getLine
       case parseString code of
         Left e -> print e >> repl
         Right expr -> case eval expr of
           Nothing -> putStrLn "Evaluation error" >> repl
           Just x -> print x >> repl

main :: IO ()
main = repl
