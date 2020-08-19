module Main where

import Parser
import Typechecker
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  code <- getContents
  let program = parseStr code
      typechecks = typecheck program
  pPrint program
  case typecheck program of
    Nothing -> putStrLn "type check succeeded"
    Just err -> putStrLn $ "type check error: " ++ err
