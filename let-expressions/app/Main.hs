module Main where

import Parser
import Compiler
import Interpreter
import Machine
import System.IO ( isEOF )
import System.Environment ( getArgs )

data CompilerMode = File | Interactive

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", fileName] -> compiler File $ Just fileName
    ["-c"] -> compiler Interactive Nothing
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

compiler :: CompilerMode -> Maybe FilePath -> IO ()
compiler Interactive Nothing = interactive handler
  where handler expr = let table = assignRegisters $ getVariables expr
                           instr = compile table expr
                           machine = initMachine $ length table
                           result = execute machine instr
                       in do
                         printList instr
                         putStrLn ""
                         print result

compiler File (Just fp) = do
  code <- readFile fp
  case parseString code of
    Left err -> print err
    Right expr -> do
      let table = assignRegisters $ getVariables expr
          instr = compile table expr
          machine = initMachine $ length table
          result = execute machine instr
      printList instr
      putStrLn ""
      print result

compiler _ _ = return ()

repl = interactive handler
  where handler expr = case eval [] expr of
                         Nothing -> putStrLn "Evaluation error"
                         Just x -> print x

usage :: IO ()
usage = putStrLn "usage: letexpressions [-c] [file]"

printList = mapM print
