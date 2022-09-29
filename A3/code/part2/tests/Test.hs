-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case (parseString "wow!") of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,


--------------------------------------------------------------------pBool--------------------------------------------------------------------
  testCase "pBool1" $ parseString "True" @?= Right [SExp (Const (TrueVal))],
  testCase "pBool2" $ parseString "False" @?= Right [SExp (Const (FalseVal))],
  testCase "pBool3" $ parseString "None" @?= Right [SExp (Const (NoneVal))],


--------------------------------------------------------------------pNum--------------------------------------------------------------------
  testCase "pNum1" $ parseString "5" @?= Right [SExp (Const (IntVal 5))],
  testCase "pNum2" $ parseString "-5" @?= Right [SExp (Const (IntVal (-5)))],
  testCase "pNum3" $ parseString "0" @?= Right [SExp (Const (IntVal 0))],
  testCase "pNum4" $ parseString "-0" @?= Right [SExp (Const (IntVal 0))],
  testCase "pNumFail1" $ case parseString "- 1" of
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pNumFail2" $ case parseString "01" of
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pNumFail3" $ case parseString "-01" of
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,


--------------------------------------------------------------------pIdent--------------------------------------------------------------------
  testCase "pIdent1" $ parseString "x" @?= Right [SExp (Var ("x"))],
  testCase "pIdent2" $ parseString "_x" @?= Right [SExp (Var ("_x"))],
  testCase "pIdent3" $ parseString "_1" @?= Right [SExp (Var ("_1"))],
  testCase "pIdent4" $ parseString "var" @?= Right [SExp (Var ("var"))],
  testCase "pIdent5" $ parseString "var1_" @?= Right [SExp (Var ("var1_"))],
  testCase "pIdent6" $ parseString "_var1_" @?= Right [SExp (Var ("_var1_"))],
  testCase "pIdent7" $ parseString "_4var1_" @?= Right [SExp (Var ("_4var1_"))],


--------------------------------------------------------------------pIdentFail--------------------------------------------------------------------
  testCase "pIdent" $ case parseString "2var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "3_var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "1x" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "if" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "None" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "True" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "False" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "for" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "if" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "in" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "not" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "1_" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "!a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "a\"" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "%a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "&a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "/a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString "(a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent" $ case parseString ")a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  

--------------------------------------------------------------------pExprOpt--------------------------------------------------------------------

  testCase "ExprOpt1" $ parseString "x < 5" @?= Right [SExp (Oper Less (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt2" $ parseString "x > 5" @?= Right [SExp (Oper Greater (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt3" $ parseString "x == 5" @?= Right [SExp (Oper Eq (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt4" $ parseString "x >= 5" @?= Right [SExp (Not (Oper Less (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt5" $ parseString "x <= 5" @?= Right [SExp (Not (Oper Greater (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt6" $ parseString "x != 5" @?= Right [SExp (Not (Oper Eq (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt7" $ parseString "x in 5" @?= Right [SExp (Oper In (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt3" $ parseString "True == False" @?= Right [SExp (Oper Eq (Const (TrueVal)) (Const (FalseVal)))],
  testCase "ExprOpt3" $ parseString "True != False" @?= Right [SExp (Oper Eq (Var "x") (Const (IntVal 5)))],
  
  testCase "ExprOptFail1" $ case parseString "1 < 2 < 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail2" $ case parseString "1 > 2 > 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail3" $ case parseString "1 == 2 == 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail4" $ case parseString "1 >= 2 >= 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail5" $ case parseString "1 <= 2 <= 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail6" $ case parseString "1 in 2 in 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail7" $ case parseString "1 != 2 != 3" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

--------------------------------------------------------------------Not--------------------------------------------------------------------

  testCase "NUUT NUUT" $ parseString "not not x" @?= Right [SExp (Not (Not (Var "x")))],
--------------------------------------------------------------------TermOpt--------------------------------------------------------------------

  testCase "TermOpt" $ parseString "x + 5" @?= Right [SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "x - 5" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "x + y" @?= Right [SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "x - y" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "0 - -5" @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt" $ parseString "0 + -5" @?= Right [SExp (Oper Plus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt" $ parseString "-0 - -5" @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt" $ case parseString "00 - -5" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

  testCase "TermOpt" $ parseString "0 + -5" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "-0 - -5" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt" $ parseString "00 - -5" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt&ExprOpt1" $ parseString "x + y < x - y" @?= Right [SExp (Oper Less (Oper Plus (Var "x") (Var "y")) (Oper Minus (Var "x") (Var "y")))],
  testCase "TermOpt&ExprOpt2" $ parseString "x * y > x / y" @?= Right [SExp (Oper Greater (Oper Times (Var "x") (Var "y")) (Oper Div (Var "x") (Var "y")))],
  testCase "TermOpt&ExprOpt3" $ parseString "x + y <= x / y" @?= Right [SExp (Not (Oper Greater (Oper Plus (Var "x") (Var "y")) (Oper Div (Var "x") (Var "y"))))],
  testCase "TermOpt&ExprOpt4" $ parseString "x * y >= x - y" @?= Right [SExp (Not (Oper Less (Oper Times (Var "x") (Var "y")) (Oper Minus (Var "x") (Var "y"))))],

{-
FactorOpt
TermOpt
  +
  -

ExprOpt
  <
  >
  ==
  >=
  <=
  in
  !=

pBool
  True
  False
  None
pIdent
  x
  _x
  _1
  var
  None
  True
  False
  for
  if
  in
  not
  1_
  !5
  "a
  %a
  &a
  /a
  (a
  )a
  x = 3 + 2
  _x = 42 + y
  _1 = 42
  var = 42

  1_ = 3 + 2
  !5
  "a
  %a
  &a

pString
pNum
pComprExp
pCClause
pExprz
pExprs


-}

  testCase "finaltest" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  ]