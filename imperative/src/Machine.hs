module Machine where

import Parser

type Index = Int

data OP = PUSH Integer -- push integer onto stack
        | LOAD Index   -- load register value to stack
        | STORE Index  -- store top of stack in register at index
        | ADD
        | SUB
        | MUL
        | DIV
        | NEG
      deriving (Eq, Show)

type Stack = [Integer]
type Registers = [Integer]

data Machine = Machine { getStack :: Stack, getRegisters :: Registers }
  deriving Show

initMachine :: Int -> Machine
initMachine nReg = Machine [] $ replicate nReg 0

getRegister :: Machine -> Index -> Integer
getRegister (Machine _ r) i = r !! i

setRegister :: Registers -> Index -> Integer -> Registers
setRegister r i x = r1 ++ (x : tail r2)
  where (r1,r2) = splitAt i r

execute :: Machine -> [OP] -> Machine
execute = foldl executeSingle

executeSingle :: Machine -> OP -> Machine
executeSingle m op = case op of
  PUSH x -> Machine s' r
    where s' = x : getStack m
          r  = getRegisters m
  LOAD i -> Machine s' r
    where s' = x : getStack m
          r = getRegisters m
          x = getRegister m i
  STORE i -> Machine xs r'
    where r' = setRegister (getRegisters m) i x
          (x:xs) = getStack m
  ADD -> Machine s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = x + y : xs
  SUB -> Machine s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = y - x : xs
  MUL -> Machine s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = x * y : xs
  DIV -> Machine s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = y `div` x : xs
  NEG -> Machine s' r
    where r = getRegisters m
          (x:xs) = getStack m
          s' = negate x : xs



