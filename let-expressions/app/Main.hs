module Main where

import Parser
import Compiler
import Interpreter
import Machine

main = do
  line <- getLine
  let tree = parseString line
      table = assignRegisters $ getVariables tree
      instr = compile table tree
      machine = initMachine $ length table
  print $ eval [] tree
  print instr
  print $ execute machine instr
