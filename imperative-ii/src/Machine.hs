module Machine
  ( OP (..)
  , initMachine
  , execute
  ) where

import Parser
import Control.Monad.State
import Control.Monad.Except

type Index = Int

data OP = PUSH Int     -- push integer onto stack
        | LOAD Index   -- load register value to stack
        | STORE Index  -- store top of stack in register at index
        | GOTO Index   -- goto pc + index unconditionally
        | CALL Index   -- goto index unconditionally
        | BZ Index     -- branch to pc + index if top of stack is zero
        | LOADPC       -- load PC + 2 onto the stack
        | STOREPC      -- store top of stack onto PC
        | HALT
        | ADD
        | SUB
        | MUL
        | DIV
        | NEG
      deriving (Eq, Show)

type Stack = [Int]
type Registers = [Int]

data Machine = Machine { getPointer :: Index
                       , getStack :: Stack
                       , getRegisters :: Registers
                       } deriving Show

type Error = String
type MachineT a = ExceptT Error (State Machine) a

execute :: Machine -> [OP] -> Either Error Machine
execute m ops =
  case runState (runExceptT (execute' ops)) m of
    (Left err, _) -> Left err
    (_, m') -> Right m'

initMachine :: Int -> Machine
initMachine nReg = Machine 0 [] $ replicate nReg 0

execute' :: [OP] -> MachineT ()
execute' ops = do
  (Machine p s r) <- get
  instr <- getInstr p ops
  case instr of
    HALT -> return ()
    _ -> executeSingle instr >> execute' ops

getInstr :: Index -> [OP] -> MachineT OP
getInstr i ops
  | i < 0 = reportError $ "Invalid PC : " ++ show i
  | i >= length ops = reportError $ "Invalid PC " ++ show i
  | otherwise = return $ ops !! i

-- execute m ops =
--   case runState (runExceptT (mapM executeSingle ops)) m of
--     (Left err, _) -> Left err
--     (_, m') -> Right m'

executeSingle :: OP -> MachineT ()

executeSingle HALT = return ()

executeSingle (PUSH i) = modify $ incrementPointer 1 . pushStack i

executeSingle (LOAD i) = do
  m <- get
  modify $ pushStack (getRegister m i)
  modify (incrementPointer 1)

executeSingle (STORE i) = do
  x <- popStack
  modify (setRegister i x)
  modify (incrementPointer 1)

executeSingle (GOTO i) =
  modify (incrementPointer i)

executeSingle (CALL i) =
  modify (setPointer i)

executeSingle (BZ i) = do
  x <- popStack
  if x == 0
     then modify (incrementPointer i)
     else modify (incrementPointer 1)

executeSingle LOADPC = do
  m <- get
  let pc = getPointer m
  modify (pushStack $ pc + 2)
  modify (incrementPointer 1)

executeSingle STOREPC = do
  pc <- popStack
  modify $ setPointer pc

executeSingle NEG = do
  x <- popStack
  modify . pushStack $ negate x
  modify (incrementPointer 1)

executeSingle DIV = do
  x <- popStack
  y <- popStack
  guard (x /= 0)
  modify . pushStack $ y `div` x
  modify (incrementPointer 1)

executeSingle op
  | op `elem` [ADD, SUB, MUL] = do
    x <- popStack
    y <- popStack
    f <- getBinOp op
    modify . pushStack $ f x y
    modify (incrementPointer 1)

getRegister :: Machine -> Index -> Int
getRegister (Machine _ _ r) i = r !! i

setRegister :: Index -> Int -> Machine -> Machine
setRegister i x m = setRegisters (r1 ++ (x : tail r2)) m
  where r = getRegisters m
        (r1,r2) = splitAt i r

setPointer :: Index -> Machine -> Machine
setPointer i (Machine _ s r) = Machine i s r

setStack :: Stack -> Machine -> Machine
setStack s (Machine p _ r) = Machine p s r

setRegisters :: Registers -> Machine -> Machine
setRegisters r (Machine p s _) = Machine p s r

pushStack :: Int -> Machine -> Machine
pushStack x (Machine p s r) = Machine p (x:s) r

incrementPointer :: Index -> Machine -> Machine
incrementPointer i (Machine p s r) = Machine (p + i) s r

popStack :: MachineT Int
popStack = do
  (Machine p s r) <- get
  case s of
    [] -> reportError "popStack: Empty stack"
    (x:xs) -> modify (setStack xs) >> return x

topOfStack :: MachineT Int
topOfStack = do
  (Machine _ s _) <- get
  case s of
    [] -> reportError "topOfStack: Empty stack"
    (x:_) -> return x

reportError :: String -> MachineT a
reportError e = do
  m <- get
  let pc = getPointer m
  throwError $ "VM Error (" ++ show pc ++ "): " ++ e

getBinOp :: OP -> MachineT (Int -> Int -> Int)
getBinOp ADD = return (+)
getBinOp SUB = return (-)
getBinOp MUL = return (*)
getBinOp op  = throwError $ "Not a binary op: " ++ show op

