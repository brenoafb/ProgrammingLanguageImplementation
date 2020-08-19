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
  if typechecks
     then putStrLn "No type errors"
     else putStrLn "Type error"
