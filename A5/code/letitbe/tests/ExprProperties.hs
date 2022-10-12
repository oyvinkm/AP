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
identN = elements ["ThisShouldBeAnUnboundVariable"]



exprN :: Int -> Gen Expr
exprN 0 = fmap Const arbitrary
exprN n = oneof
   [fmap Const arbitrary
   , do x <- exprN (n `div` 2)
        y <- exprN (n `div` 2)
        op <- opN
        return $ Oper op x y
   , do ident <- identN
        return (Var ident)
   , do e1 <- exprN (n `div` 2)
        e2 <- exprN (n `div` 2)
        ident <- identN
        return $ Let ident e1 e2
   ]

instance Arbitrary Expr where
   arbitrary = sized exprN

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = (E.evalTop (E.simplify x)) === (E.evalTop x)



-- Questions how do we compile the exprprop?
-- we moved the files into the ExprAst into test folder
-- How do we handle the case where the generator chooses to reference a variable
-- that hasn't been bound
