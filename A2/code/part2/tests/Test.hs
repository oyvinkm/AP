-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),

---------Look---------
    testCase "OPERATE 1" $ operate Plus (IntVal 2) (IntVal 1) @?= Right (IntVal 3),
    testCase "OPERATE 1" $ operate Plus (IntVal -2) (IntVal 1) @?= Right (IntVal (-1)),
    testCase "OPERATE 1" $ operate Plus (IntVal 5) (StringVal "2") @?= Left "Error, the operator couldn't handle the arguments."]
    testCase "OPERATE 2" $ operate Plus (IntVal 0) (IntVal 0) @?= Right (IntVal 0)



--  Left "Cannot divide by zero"
--  Left "Cannot do mod by zero"
--  Left "Error, the operator couldn't handle the arguments."