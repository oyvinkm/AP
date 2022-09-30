-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $ parseString "2 + two" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $ case (parseString "wow!") of
      Left e -> return ()
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
  testCase "pIdent7" $ parseString "_4var1_" @?= Right [SExp (Var ("_4var1_"))],


--------------------------------------------------------------------pIdentFail--------------------------------------------------------------------
  testCase "pIdent1" $ case parseString "2var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent2" $ case parseString "3_var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent3" $ case parseString "1x" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent8" $ case parseString "for" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent9" $ case parseString "if" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent10" $ case parseString "in" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent11" $ case parseString "not" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent12" $ case parseString "1_" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent13" $ case parseString "!a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent14" $ case parseString "a\"" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent15" $ case parseString "%a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent16" $ case parseString "&a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent17" $ case parseString "/a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent18" $ case parseString "(a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pIdent19" $ case parseString ")a" of  
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
  testCase "ExprOpt8" $ parseString "True == False" @?= Right [SExp (Oper Eq (Const (TrueVal)) (Const (FalseVal)))],
  testCase "ExprOpt9" $ parseString "True != False" @?= Right [SExp (Not (Oper Eq (Const (TrueVal)) (Const (FalseVal))))],
  
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

  testCase "TermOpt1" $ parseString "x + 5" @?= Right [SExp (Oper Plus (Var "x") (Const (IntVal 5)))],
  testCase "TermOpt2" $ parseString "x - 5" @?= Right [SExp (Oper Minus (Var "x") (Const (IntVal 5)))],
  testCase "TermOpt3" $ parseString "x + y" @?= Right [SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "TermOpt4" $ parseString "x - y" @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt5" $ parseString "0 - -5" @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt6" $ parseString "0 + -5" @?= Right [SExp (Oper Plus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt7" $ parseString "-0 - -5" @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt8" $ case parseString "00 - -5" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "TermOpt8" $ case parseString "5 - -001" of  
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

--------------------------------------------------------------------FactorOpt--------------------------------------------------------------------

  testCase "FactorOpt1" $ parseString "10 * -5" @?= Right [SExp (Oper Times (Const (IntVal 10)) (Const (IntVal (-5))))],
  testCase "FactorOpt2" $ parseString "-0 // -5" @?= Right [SExp (Oper Div (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "FactorOpt3" $ parseString "20 // 3" @?= Right [SExp (Oper Div (Const (IntVal 20)) (Const (IntVal (3))))],
  testCase "FactorOpt4" $ parseString "20 // 0" @?= Right [SExp (Oper Div (Const (IntVal 20)) (Const (IntVal (0))))],
  testCase "FactorOpt5" $ parseString "20 % 40" @?= Right [SExp (Oper Mod (Const (IntVal 20)) (Const (IntVal (40))))],


--------------------------------------------------------------------FactorTermExprOpt--------------------------------------------------------------------

  testCase "FactorOpt&TermOpt&ExprOpt1" $ parseString "x + y < x - y" @?= Right [SExp (Oper Less (Oper Plus (Var "x") (Var "y")) (Oper Minus (Var "x") (Var "y")))],
  testCase "FactorOpt&TermOpt&ExprOpt2" $ parseString "x * y > x // y" @?= Right [SExp (Oper Greater (Oper Times (Var "x") (Var "y")) (Oper Div (Var "x") (Var "y")))],
  testCase "FactorOpt&TermOpt&ExprOpt3" $ parseString "x + y <= x // y" @?= Right [SExp (Not (Oper Greater (Oper Plus (Var "x") (Var "y")) (Oper Div (Var "x") (Var "y"))))],
  testCase "FactorOpt&TermOpt&ExprOpt4" $ parseString "x * y >= x - y" @?= Right [SExp (Not (Oper Less (Oper Times (Var "x") (Var "y")) (Oper Minus (Var "x") (Var "y"))))],


--------------------------------------------------------------------pString--------------------------------------------------------------------

  testCase "pString1" $ parseString "'This is a string'" @?= Right [SExp (Const (StringVal ("This is a string")))],
  testCase "pString1" $ parseString "''" @?= Right [SExp (Const (StringVal ("")))],
  testCase "pString2" $ parseString "'This is a \n string'" @?= Right [SExp (Const (StringVal ("This is a string")))],
  testCase "pString3" $ parseString "'This is \t a string'" @?= Right [SExp (Const (StringVal ("This is \t a string")))],
  testCase "pString4" $ parseString "'This is \\ a string'" @?= Right [SExp (Const (StringVal ("This is \\ a string")))],
  testCase "pString5" $ parseString "'This is \\\' a string'" @?= Right [SExp (Const (StringVal ("This is \' a string")))],
  testCase "pString6" $ parseString "'This is a com#ment'" @?= Right [SExp (Const (StringVal ("This is a com")))],

  testCase "pString7" $ case parseString "'This is no#t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString7" $ case parseString "'This is no#t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString8" $ case parseString "'This is not \\\a string'" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString9" $ case parseString "'This is not \a string'" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString7" $ case parseString "'This is no§t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString7" $ case parseString "'This is no¾t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "pString7" $ case parseString "'This is no½t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

--------------------------------------------------------------------Compr--------------------------------------------------------------------

  testCase "Compr" $ parseString "[x for x in x]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "[xfor y in z]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "[x for x in x]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "[x for x in x]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "[x for x in x if u]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x"), CCIf (Var "u")])],
  testCase "Compr" $ parseString "[x for x in x in u]" @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x"), CCIf (Var "u")])],
  testCase "Compr" $ parseString "[x+2 for x in x for y in 7 if 1 < 2]" @?= Right [SExp (Compr (Oper Plus (Var "x") (Const (IntVal 2))) [CCFor "x" (Var "x"),CCFor "y" (Const (IntVal 7)),CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "ComprFail" $ case parseString "[x for x in x ift]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Comprfail" $ case parseString "[xfor y in z]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "Comprfail" $ case parseString "[x forz in k]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,


--------------------------------------------------------------------pExprs--------------------------------------------------------------------

  testCase "Compr" $ parseString "5; 4" @?= Right [SExp (Const (IntVal 5)), SExp (Const (IntVal 4))],
  testCase "Compr" $ parseString "5 + x; 4" @?= Right [SExp (Oper Plus (Const (IntVal 5)) (Var "x")),SExp (Const (IntVal 4))],
  testCase "Compr" $ parseString "'This is a string'; 4" @?= Right [SExp (Const (StringVal "This is a string")),SExp (Const (IntVal 4))],
  testCase "Compr" $ parseString "'This is a string'; 4; [x for x in x]" @?= Right [SExp (Const (StringVal "This is a string")),SExp (Const (IntVal 4)), SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "'This is a string'; 4 % 8; [x for x in x]" @?= Right [SExp (Const (StringVal "This is a string")),SExp (Oper Mod (Const (IntVal 4)) (Const (IntVal 8))),SExp (Compr (Var "x") [CCFor "x" (Var "x")])],
  testCase "Compr" $ parseString "x=5; x+2" @?= Right [SDef "x" (Const (IntVal 5)),SExp (Oper Plus (Var "x") (Const (IntVal 2)))],
  testCase "Compr" $ parseString "x=5; y=7; x+y" @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "Compr" $ parseString "x=5; y=7; [x for x in y if 1 < 2]" @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "Compr" $ parseString "x = 5; y = 7; [x for x in y if 1 < 2]" @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "Compr" $ parseString "x = 5; y = 7*x; [x for x in y if 1 < 2]" @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Oper Times (Const (IntVal 7)) (Var "x")),SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "Compr" $ parseString "x = 5; y = 7; [x      for x    in y     if 1  < 2    ]" @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],







  
  




{-
FactorOpt
  *
  //
  %
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

pString
  done
  done
pNum
  5
  -5
  0
  -0
  - 1
  00
  -00
pComprExp
  Some
  what 
  Tested

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
  Vi mangler =
pCClause
pExprz
pExprs


-}

  testCase "finaltest" $ parseString "1" @?= Right [SExp (Const (IntVal 1))]
  ]