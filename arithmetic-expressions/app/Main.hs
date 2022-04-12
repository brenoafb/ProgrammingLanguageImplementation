module Main where

import Parser
import Interpreter
import Compiler
import Machine

import System.IO ( isEOF )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", fileName] -> compiler fileName
    ["-v", fileName] -> vm fileName
    [] -> repl
    _ -> usage

interactive handler = do
  eof <- isEOF
  if eof
     then return ()
     else do
       code <- getLine
       case parseString code of
         Left e -> print e >> interactive handler
         Right expr -> handler expr >> interactive handler

compiler :: FilePath -> IO ()
compiler fp = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right expr -> do
      compileASM expr

vm :: FilePath -> IO ()
vm fp = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right expr -> do
      let program = compile expr
          result = execute [] program
      printList program
      putStrLn ""
      print result

repl = interactive handler
  where handler expr = case eval expr of
                         Nothing -> putStrLn "Evaluation error"
                         Just x -> print x

usage :: IO ()
usage = putStrLn "usage: calculator [-c] [file]"

printList = mapM print
