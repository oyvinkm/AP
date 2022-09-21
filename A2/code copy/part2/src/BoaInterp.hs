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

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f = Comp (\r -> case runComp m r of 
                            (Left e, s) -> (Left e, s)
                            (Right a, s) -> let (x1, s1) = runComp (f a) r
                                            in (x1, s <> s1))


  -- m >>= f = Comp (\r -> do (Right b, s1) <- runComp m r
  --                          (Right c, s2) <- runComp (f (Right b)) r
  --                          return (c, s1 <> s2))

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
withBinding x v m = Comp (\r -> let r' = r ++ [(x, v)] in runComp m r')

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s] ++ mempty)) -- Do you have to put mempty??

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
operate Div (IntVal v1) (IntVal v2) = Right (IntVal (v1 `div` v2))
operate Mod (IntVal v1) (IntVal v2) = Right (IntVal (v1 `mod` v2))
operate Eq v1 v2 = if v1 == v2 then (Right TrueVal)
                      else (Right FalseVal)
operate Less  (IntVal v1) (IntVal v2) = if v1 < v2 then Right TrueVal
                      else Right FalseVal
operate Greater (IntVal v1) (IntVal v2) = if v1 > v2 then Right TrueVal
                      else Right FalseVal
operate In v1 (ListVal v2) = if v1 `elem` v2 then Right TrueVal
                            else Right FalseVal

operate _ _ _ = Left "Error, the operator couldn't handle the arguments."

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
                        IntVal _ -> True && isIntList xs
                        _ -> False
isIntList [] = True

concatStrings :: [Value] -> String
concatStrings [] = "" 
{- concatStrings (x:[]) = let res = case v of
                                    NoneVal -> "None"
                                    IntVal a -> show a
                                    TrueVal -> show True
                                    FalseVal -> show False 
                                    StringVal s -> s 
                                    ListVal (x:xs) -> "[" ++ concatStrings [x] ++ "," ++ concatStrings xs ++ "]" -- prints comma after last element due to [x] = x:[]
                                    ListVal [] -> "[]"
                        in res -}
concatStrings (v:vs) = let res = case v of
                                    NoneVal -> "None"
                                    IntVal a -> show a
                                    TrueVal -> show True
                                    FalseVal -> show False 
                                    StringVal s -> s 
                                    ListVal (x:xs) -> "[" ++ concatStrings [x] ++ "," ++ concatStrings xs ++ "]" -- prints comma after last element due to [x] = x:[]
                                    ListVal [] -> "[]"
                        in res ++ case vs of 
                                    [] -> ""
                                    _ -> " " ++ concatStrings vs

apply :: FName -> [Value] -> Comp Value
apply "range" vs = if (checkLen vs && isIntList vs) then 
                    case vs of
                      [IntVal a] -> return (ListVal (map IntVal [0 .. a-1]))
                      [IntVal a, IntVal b] -> return (ListVal (map IntVal [a .. b-1]))
                      [IntVal a, IntVal b, IntVal c] -> return (ListVal (map IntVal [a, a+c .. b-1]))
                      _ -> undefined
                    else abort (EBadArg "Argument is invalid.")
apply "print" vs = do _ <- output $ concatStrings vs; return (NoneVal)
apply f _ = abort (EBadFun f)
-- apply "print" (v:vs) = show v ++ apply "print" vs



-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined



-- ListOfFalse = [None, False, 0, "", []]

---------TESTS
-- truthy (ListVal [])
-- operate Plus (IntVal 4) (IntVal (-1))
-- operate Mod (IntVal 40) (IntVal (3))
-- operate Mod (IntVal 4) (IntVal (-1))
-- concatStrings [NoneVal, IntVal 4, TrueVal, FalseVal, StringVal "Øyvin <3", ListVal [IntVal 4, NoneVal, ListVal [StringVal "Yo"]]]
-- concatStrings [ListVal []]