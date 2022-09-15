-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst e1) = show e1
showExp (Add e1 e2) = "(" ++ showExp e1 ++ ")" ++ "+" ++ "(" ++ showExp e2 ++ ")" 
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ ")"++ "-" ++ "(" ++ showExp e2 ++ ")" 
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ ")" ++ "*" ++ "(" ++ showExp e2 ++ ")" 
showExp (Div e1 e2) = "(" ++ showExp e1 ++ ")" ++ "`div`" ++ "(" ++ showExp e2 ++ ")" 
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ ")" ++ "^" ++ "(" ++ showExp e2 ++ ")"
showExp e1 = show e1

evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) = evalSimple e1 `div` evalSimple e2
-- `seq` was used to ensure the evaluation of e1 ahead of e2
evalSimple (Pow e1 e2) = let x = evalSimple e1 in x `seq` x ^ evalSimple e2
evalSimple _ = undefined

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \var -> if var == v then Just n else r var

evalFull :: Exp -> Env -> Integer
evalFull (If test yes no) r = if evalFull test r /= 0 then evalFull yes r else evalFull no r
evalFull (Var v) r = case r v of
                        Just x -> x
                        Nothing -> undefined
evalFull (Let var def body) r =  evalFull body (extendEnv var (evalFull def r) r)
evalFull (Add e1 e2) r = evalFull e1 r + evalFull e2 r
evalFull (Sub e1 e2) r = evalFull e1 r - evalFull e2 r
evalFull (Mul e1 e2) r = evalFull e1 r * evalFull e2 r
evalFull (Div e1 e2) r = evalFull e1 r `div` evalFull e2 r
evalFull (Pow e1 e2) r = let x = evalFull e1 r in x `seq` x ^ evalFull e2 r
evalFull (Cst e1) _ = e1
evalFull (Sum var from to body) r = if evalFull from r > evalFull to r then 0 else
                                        evalFull (Add body (Sum var (Add from (Cst 1)) to body)) 
                                        (extendEnv var (evalFull from r) r)

evalErr :: Exp -> Env -> Either ArithError Integer
{-Monads were used due to their ability to handle side effects, 
e.g. if one of the expressions evaluates to an error-}
evalErr (Cst a) _ = Right a
evalErr (Var v) r = case r v of
                      Nothing -> Left (EBadVar v)
                      Just a -> Right a
evalErr (Add e1 e2) r = evalErr e1 r >>= \x -> evalErr e2 r >>= \y -> return $ x + y
evalErr (Sub e1 e2) r = evalErr e1 r >>= \x -> evalErr e2 r >>= \y -> return $ x - y
evalErr (Mul e1 e2) r = evalErr e1 r >>= \x -> evalErr e2 r >>= \y -> return $ x * y
evalErr (Div e1 e2) r = evalErr e1 r >>= \x -> 
                        evalErr e2 r >>= \y -> if y == 0 
                            then Left EDivZero else return $ x `div` y
evalErr (Pow e1 e2) r = evalErr e1 r >>= \x -> 
                            evalErr e2 r >>= \y -> if y < 0 
                                then Left ENegPower else return $ x ^ y
evalErr (If t y n) r = evalErr t r >>= \x -> if x /= 0 then 
                            evalErr y r >>= \z -> return z else evalErr n r >>= \k -> return k
evalErr (Let var def body) r = evalErr def r >>= \x -> evalErr body (extendEnv var x r)
evalErr (Sum var from to body) r = evalErr from r >>= \x -> evalErr to r  >>= \y -> 
                                    if x > y then return 0 
                                        else evalErr (Add body (Sum var (Add from (Cst 1)) to body)) 
                                        (extendEnv var x r)


-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either e1rithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either e1rithError Integer
evalLazy = undefined