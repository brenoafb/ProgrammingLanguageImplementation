module Compiler
  ( compile
  , compileASM
  ) where

import Machine
import Parser
import System.Process

compile :: Expr -> Program
compile expr = case expr of
                 Num x -> [PUSH x]
                 Neg e -> compile e <> [NEG]
                 _ -> let c1 = compile $ e1 expr
                          c2 = compile $ e2 expr
                          op = getOp expr
                       in c1 <> c2 <> [op]

getOp :: Expr -> Op
getOp (Mult _ _) = MUL
getOp (Div _ _)  = DIV
getOp (Add _ _)  = ADD
getOp (Sub _ _)  = SUB
getOp (Neg _)    = NEG
getOp _ = undefined

asmHeader =
  unlines [ "    .text"
          , "    .p2align 4,,15"
          , ".global entry"
          , "    .type entry, @function"
          , "entry:"
          ]

compileASM :: Expr -> IO ()
compileASM expr = do
  let asm = emitASM expr
      output = asmHeader <> unlines (map (<> "    ") $ asm <> ["ret"])
      filename = "output.s"
  writeFile filename output
  bundle filename

bundle :: FilePath -> IO ()
bundle filename = do
  readProcessWithExitCode "gcc" (words $ "-g assets/driver.c " <> filename <> " --omit-frame-pointer -o main") ""
  pure ()


emitASM (Num x) =
  ["movq $" <> show x <> ", %rax"]
emitASM (Add e1 e2) =
     emitASM e1
  <> ["push %rax"]
  <> emitASM e2
  <> ["pop %rbx"]
  <> ["addq %rbx, %rax"]
emitASM (Mult e1 e2) =
     emitASM e1
  <> ["push %rax"]
  <> emitASM e2
  <> ["pop %rbx"]
  <> ["imulq %rbx, %rax"]
emitASM (Sub e1 e2) =
     emitASM e2
  <> ["push %rax"]
  <> emitASM e1
  <> ["pop %rbx"]
  <> ["subq %rbx, %rax"]
emitASM (Div e1 e2) =
     emitASM e2
  <> ["push %rax"]
  <> emitASM e1
  <> ["pop %rbx"]
  <> ["cqto"]
  <> ["idivq %rbx"]
