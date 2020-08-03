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
execute [] [] = Nothing
execute [x] [] = Just x
execute _ [] = Nothing
execute st (PUSH x:ops) = execute (x:st) ops
execute (x:y:xs) (op:ops) = case op of
                              MUL -> execute (x * y : xs) ops
                              DIV -> execute (x `div` y : xs) ops
                              ADD -> execute (x + y : xs) ops
                              SUB -> execute (x - y : xs) ops
                              NEG -> execute ((-x) : y : xs) ops
                              _   -> Nothing
execute (x:xs) (NEG:ops) = execute ((-x) : xs) ops
execute _ _ = Nothing

