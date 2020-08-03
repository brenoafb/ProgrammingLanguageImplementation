module Machine where

data Op = PUSH Integer
        | MUL
        | DIV
        | ADD
        | SUB

type Program = [Op]

execute :: Program
execute = undefined
