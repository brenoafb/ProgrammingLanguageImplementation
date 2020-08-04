module Interpreter where

import Parser

type Env = [(String, Integer)]

eval :: Env -> Expr -> Maybe Integer
eval env (Num x) = Just x
eval env (Var s) = lookup s env
eval env (Let p e) = do
  p' <- traverse (\(v,e) -> (\x -> (v,x)) <$> eval env e) p
  let env' = p' ++ env
  eval env' e
eval env (Neg e) = negate <$> eval env e
eval env expr = op <$> eval env (e1 expr) <*> eval env (e2 expr)
  where op = getOp expr

getOp :: Expr -> (Integer -> Integer -> Integer)
getOp (Add _ _) = (+)
getOp (Sub _ _) = (-)
getOp (Mult _ _) = (*)
getOp (Div _ _) = div

update :: Env -> String -> Integer -> Env
update (p@(v1,x1):ps) v2 x2 = if v1 == v2 then (v1,x2):ps
                                          else p:update ps v2 x2
update [] v2 x2 = [(v2,x2)]
