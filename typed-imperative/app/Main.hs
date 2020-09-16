module Main where

import Parser
import Typechecker
import Interpreter
import Text.Pretty.Simple (pPrint)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    [filename] -> executeProgram filename
    ["-t", filename] -> typecheckProgram filename
    ["-p", filename] -> printAST filename

executeProgram :: FilePath -> IO ()
executeProgram file = do
  code <- readFile file
  let program = parseStr code
      typechecks = typecheck program
  case typecheck program of
    Just err -> putStrLn $ "Type check error: " ++ err
    Nothing -> do
      putStrLn "type check succeeded"
      let eval = runProgram program
      case eval of
        Left err -> putStrLn $ "Interpreter error: " ++ err
        Right env -> print env

typecheckProgram :: FilePath -> IO ()
typecheckProgram file = do
  code <- readFile file
  let program = parseStr code
      typechecks = typecheck program
  case typecheck program of
    Just err -> putStrLn $ "Type check error: " ++ err
    Nothing -> return ()

printAST :: FilePath -> IO ()
printAST file = do
  code <- readFile file
  let ast = parseStr code
  pPrint ast
