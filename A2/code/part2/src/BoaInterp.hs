-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

-- Check if length of list is in range [1,3]
checkLen :: [Value] -> Bool
checkLen vs = case operate Less  (IntVal (length vs)) (IntVal 4) of
                Right TrueVal -> case vs of
                                  [] -> False
                                  _ -> True
                Right FalseVal -> False
                Right _ -> False
                Left _ -> False

-- Checks if all elements in list are type IntVal, should not be called with empty list.
isIntList :: [Value] -> Bool
isIntList (x:xs) = case x of
                        IntVal _ -> isIntList xs
                        _ -> False
isIntList [] = True

listToString :: [Value] -> String
listToString [] = ""
listToString (x:xs) = let res = case x of
                                  NoneVal -> "None"
                                  IntVal a -> show a
                                  TrueVal -> show True
                                  FalseVal -> show False 
                                  StringVal s -> s 
                                  ListVal (x:xs) -> "[" ++ listToString [x] ++ case xs of 
                                                                                  [] -> listToString xs ++ "]"
                                                                                  _ -> ", " ++ listToString xs ++ "]"
                                  ListVal [] -> "[]"
                      in res ++ case xs of
                                   [] -> "" 
                                   _ -> ", " ++ listToString xs

concatStrings :: [Value] -> String
concatStrings [] = "" 
concatStrings (v:vs) = let res = case v of
                                    NoneVal -> "None"
                                    IntVal a -> show a
                                    TrueVal -> show True
                                    FalseVal -> show False 
                                    StringVal s -> s 
                                    ListVal xs -> "[" ++ listToString xs ++ "]"
                        in res ++ case vs of
                                   [] -> "" ++ concatStrings vs
                                   _ -> " " ++ concatStrings vs
newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f = Comp (\r -> case runComp m r of 
                            (Left e, s) -> (Left e, s)
                            (Right a, s) -> let (x1, s1) = runComp (f a) r
                                            in (x1, s <> s1))

-- You shouldn't need to modify these r : (x, v)
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\_ -> (Left re, mempty))

look :: VName -> Comp Value
look x = Comp (\r -> case lookup x r of
                      Nothing -> (Left (EBadVar x), mempty)
                      Just a -> (Right a, mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp (\r -> runComp m ((x,v):r))

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s])) -- Do you have to put mempty??

-- Helper functions for interpreter, None, False, 0, "", [], are considered false
truthy :: Value -> Bool
truthy v = case v of
            IntVal 0 -> False
            StringVal "" -> False
            ListVal [] -> False
            NoneVal -> False
            FalseVal -> False
            _ -> True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
operate Div (IntVal v1) (IntVal v2) = case v2 of
                                        0 -> Left "divide by zero"
                                        _ -> Right (IntVal (v1 `div` v2))
operate Mod (IntVal v1) (IntVal v2) = case v2 of
                                        0 -> Left "mod by zero"
                                        _ -> Right (IntVal (v1 `mod` v2))
operate Eq v1 v2 = if v1 == v2 then Right TrueVal
                      else Right FalseVal
operate Less  (IntVal v1) (IntVal v2) = if v1 < v2 then Right TrueVal
                      else Right FalseVal
operate Greater (IntVal v1) (IntVal v2) = if v1 > v2 then Right TrueVal
                      else Right FalseVal
operate In v1 (ListVal v2) = if v1 `elem` v2 then Right TrueVal
                            else Right FalseVal

operate _ _ _ = Left "Error, the operator couldn't handle the arguments."


apply :: FName -> [Value] -> Comp Value
apply "range" vs = if checkLen vs && isIntList vs then 
                    case vs of
                      [IntVal a] -> return (ListVal (map IntVal [0 .. a-1]))
                      [IntVal a, IntVal b] ->  return (ListVal (map IntVal [a .. b-1]))
                      [IntVal a, IntVal b, IntVal c] -> if (a > b && c>0) || (a < b && c < 0) then return(ListVal [])
                                                        else return (ListVal (map IntVal [a, a+c .. b - (c `div` abs c)]))
                      _ -> undefined
                    else abort (EBadArg "Argument is invalid.")
apply "print" vs = do _ <- output $ concatStrings vs; return NoneVal
apply f _ = abort (EBadFun f)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var x) = look x
eval (Oper op e1 e2) = do x <- eval e1
                          y <- eval e2
                          case operate op x y of
                            Right a -> return a
                            Left e -> abort $ EBadArg e
eval (Not e) = do a <- eval e
                  if truthy a then return FalseVal 
                    else return TrueVal
eval (Call f es) = do vals <- mapM eval es
                      apply f vals
eval (List es) = do xs <- mapM eval es
                    return (ListVal xs)
eval (Compr e cs) = case cs of
    [] -> do v1 <- eval e; return $ ListVal [v1]
    (c : cs) -> case c of
                  CCFor v x -> do 
                            x1 <- eval x
                            case x1 of 
                              ListVal l -> do
                                x2 <- mapM(\value -> withBinding v value (eval(Compr e cs))) l;
                                  return (ListVal (concatMap(\(ListVal ss) -> ss) x2))
                              _ -> abort $ EBadArg "Argument is invalid"
                  CCIf x -> do v1 <- eval x; if truthy v1 
                                              then eval $ Compr e cs
                                             else return $ ListVal []

exec :: Program -> Comp ()
exec [] = return ()
exec (y:ys) = case y of
                SDef v e -> do res <- eval e
                               withBinding v res (exec ys)
                SExp e -> do _ <- eval e 
                             exec ys


execute :: Program -> ([String], Maybe RunError)
execute p = let (eev, strs) = runComp (exec p) []
            in case eev of 
              Right () -> (strs, Nothing)
              Left err -> (strs, Just err)



---------TESTS
-- truthy (ListVal [])
-- operate Plus (IntVal 4) (IntVal (-1))
-- operate Mod (IntVal 40) (IntVal (3))
-- operate Mod (IntVal 4) (IntVal (-1))
-- concatStrings [NoneVal, IntVal 4, TrueVal, FalseVal, StringVal "Øyvin <3", ListVal [IntVal 4, NoneVal, ListVal [StringVal "Yo"]]]
-- concatStrings [ListVal []]
-- [[1,2],None,[]]
-- concatStrings [ListVal[ListVal[IntVal 1, IntVal2], NoneVal, ListVal[]]]


-- ccHelper :: CClause -> Exp -> Comp [Value]
-- ccHelper (CCFor v ec) e = do
--                         l <- eval ec
--                         case l of
--                           ListVal (x:xs) -> return [withBinding v x (eval e) | x <- xs]
--                           ListVal [] -> return (ListVal [])
--                           _ ->  abort (EBadArg "Argument is invalid.")