module Machine where

data Op = PUSH Integer
        | MUL
        | DIV
        | ADD
        | SUB
        | NEG
        deriving Show

type Program = [Op]

execute :: [Integer] -> Program -> Maybe Integer
execute [x] [] = Just x -- normal finalization
execute [] [] = Nothing -- no item in stack upon finalizing
execute _ [] = Nothing -- more than one item in stack upon finalizing
execute st (PUSH x:ops) = execute (x:st) ops
execute (x:y:xs) (op:ops) = case op of
                              MUL -> execute (x * y : xs) ops
                              DIV -> execute (y `div` x : xs) ops
                              ADD -> execute (x + y : xs) ops
                              SUB -> execute (y - x : xs) ops
                              NEG -> execute ((-x) : y : xs) ops
                              _   -> Nothing
execute (x:xs) (NEG:ops) = execute ((-x) : xs) ops
execute _ _ = Nothing

