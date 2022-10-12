module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) env = return n
eval (Oper op x y) env = (oper op) <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop e = eval e M.empty

contains :: Ident -> Expr -> Bool -> Bool
contains v body b = 
  case body of
    Oper _ (Const _) (Const _) -> False
    Var v -> True  
    Oper op e1 e2 -> (contains v e1 b) || (contains v e2 b)
    Let x e body -> contains v body b
    _ -> False

simplify e =
  case e of
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    
    Oper Times (Const 0) (Const _) -> Const 0
    Oper Times (Const _) (Const 0) -> Const 0
    Oper Times (Const c1) (Const 1) -> Const c1
    Oper Times (Const 1) (Const c2) -> Const c2

    Oper Times (Const c1) (Const (-1)) -> Const (-c1)
    Oper Times (Const (-1)) (Const c2) -> Const (-c2)
    Var v -> Var v
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)

    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body -> case contains v body False of
                      False -> simplify body
                      True -> Let v (simplify e) (simplify body)
    _ -> e
