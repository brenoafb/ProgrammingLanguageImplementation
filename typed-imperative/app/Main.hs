module Main where

import Parser
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  code <- getContents
  let tree = parseStr code
  pPrint tree
