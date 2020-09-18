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
        | GOTO Index   -- goto index unconditionally
        | BZ Index     -- branch if top of stack is zero
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
  | i < 0 = throwError "Invalid PC"
  | i >= length ops = throwError "Invalid PC"
  | otherwise = return $ ops !! i

-- execute m ops =
--   case runState (runExceptT (mapM executeSingle ops)) m of
--     (Left err, _) -> Left err
--     (_, m') -> Right m'

executeSingle :: OP -> MachineT ()

executeSingle HALT = return ()

executeSingle (PUSH i) = modify $ pushStack i

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

executeSingle (BZ i) = do
  x <- popStack
  if x == 0
     then modify (incrementPointer i)
     else modify (incrementPointer 1)

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
    [] -> throwError "Empty stack"
    (x:xs) -> modify (setStack xs) >> return x

topOfStack :: MachineT Int
topOfStack = do
  (Machine _ s _) <- get
  case s of
    [] -> throwError "Empty stack"
    (x:_) -> return x

getBinOp :: OP -> MachineT (Int -> Int -> Int)
getBinOp ADD = return (+)
getBinOp SUB = return (-)
getBinOp MUL = return (*)
getBinOp op  = throwError $ "Not a binary op: " ++ show op

