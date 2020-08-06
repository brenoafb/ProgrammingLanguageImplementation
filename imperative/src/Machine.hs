module Machine where

import Parser

type Index = Int

data OP = PUSH Int     -- push integer onto stack
        | LOAD Index   -- load register value to stack
        | STORE Index  -- store top of stack in register at index
        | GOTO Index   -- goto index unconditionally
        | BZ Index     -- branch if top of stack is zero
        | ADD
        | SUB
        | MUL
        | DIV
        | NEG
      deriving (Eq, Show)

type Stack = [Int]
type Registers = [Int]

data Machine = Machine { getPointer :: Index, getStack :: Stack, getRegisters :: Registers }
  deriving Show

initMachine :: Int -> Machine
initMachine nReg = Machine 0 [] $ replicate nReg 0

getRegister :: Machine -> Index -> Int
getRegister (Machine _ _ r) i = r !! i

setRegister :: Registers -> Index -> Int -> Registers
setRegister r i x = r1 ++ (x : tail r2)
  where (r1,r2) = splitAt i r

execute :: Machine -> [OP] -> Machine
execute = foldl executeSingle

-- highly repetitive code ahead...
executeSingle :: Machine -> OP -> Machine
executeSingle m op = case op of
  PUSH x -> Machine p s' r
    where s' = x : getStack m
          r  = getRegisters m
          p  = 1 + getPointer m
  LOAD i -> Machine p s' r
    where s' = x : getStack m
          r = getRegisters m
          x = getRegister m i
          p  = 1 + getPointer m
  STORE i -> Machine p xs r'
    where r' = setRegister (getRegisters m) i x
          (x:xs) = getStack m
          p  = 1 + getPointer m
  GOTO i -> Machine i s r
    where r = getRegisters m
          s = getStack m
  BZ i -> Machine p xs r
    where r = getRegisters m
          x:xs = getStack m
          p = if x == 0 then i else getPointer m
  ADD -> Machine p s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = x + y : xs
          p  = 1 + getPointer m
  SUB -> Machine p s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = y - x : xs
          p  = 1 + getPointer m
  MUL -> Machine p s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = x * y : xs
          p  = 1 + getPointer m
  DIV -> Machine p s' r
    where r = getRegisters m
          (x:y:xs) = getStack m
          s' = y `div` x : xs
          p  = 1 + getPointer m
  NEG -> Machine p s' r
    where r = getRegisters m
          (x:xs) = getStack m
          s' = negate x : xs
          p  = 1 + getPointer m



