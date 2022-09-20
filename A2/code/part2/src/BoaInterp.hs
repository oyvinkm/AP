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
operate = undefined

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined



-- ListOfFalse = [None, False, 0, "", []]