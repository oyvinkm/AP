module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E

-- To run it, stand in ~/A5/code/letitbe and do 
     -- $ stack ghci --ghc-options -W
     -- $ :l tests/ExprProperties

-- Generates random operation
opN :: Gen Op
-- opN = oneof [return Plus, return Minus, return Times]
opN = elements [Plus, Minus, Times]

identN :: Gen String
identN = elements ["a", "b", "c"]

genList :: Gen String
genList = do 
     n <- chooseInt(1,2)
     str <- vectorOf n (elements ['A' .. 'Z'])
     return $ str



exprN :: [String] -> Int -> Gen Expr
exprN _ 0 = fmap Const arbitrary
exprN xs n = frequency[(2, fmap Const arbitrary)
   , (3, do x <- exprN xs (n `div` 2)
            y <- exprN xs (n `div` 2)
            op <- opN
            return $ Oper op x y)
   , (1, do ident <- elements xs
            return (Var ident))
   , (2, do ident <- genList 
            e1 <- exprN (xs ++ [ident]) (n `div` 2)
            e2 <- exprN (xs ++ [ident]) (n `div` 2)
            return $ Let ident e1 e2)
   ]

instance Arbitrary Expr where
   arbitrary =  sized (exprN ["A"])

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = (E.evalTop (E.simplify x)) === (E.evalTop x)



-- Questions how do we compile the exprprop?
-- we moved the files into the ExprAst into test folder
-- How do we handle the case where the generator chooses to reference a variable
-- that hasn't been bound
