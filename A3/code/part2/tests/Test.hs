-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ atomTests

atomTests = testGroup "Minimal pAtom" [
--------------------------------------------------------------------pBool--------------------------------------------------------------------
  testCase "pBool1" $ parseString "True" 
    @?= Right [SExp (Const (TrueVal))],
  testCase "pBool2" $ parseString "False" 
    @?= Right [SExp (Const (FalseVal))],
  testCase "pBool3" $ parseString "None" 
    @?= Right [SExp (Const (NoneVal))],


--------------------------------------------------------------------pNum--------------------------------------------------------------------
  testCase "pNum1" $ parseString "5" 
    @?= Right [SExp (Const (IntVal 5))],
  testCase "pNum2" $ parseString "5593" 
    @?= Right [SExp (Const (IntVal 5593))],
  testCase "pNum3" $ parseString "-5"

    @?= Right [SExp (Const (IntVal (-5)))],
  testCase "pNum4" $ parseString "-554328" 
    @?= Right [SExp (Const (IntVal (-554328)))],
  testCase "pNum5" $ parseString "0" 
    @?= Right [SExp (Const (IntVal 0))],
  testCase "pNum6" $ parseString "-0" 
    @?= Right [SExp (Const (IntVal 0))],
  testCase "pNumFail1" $ case parseString "- 1" of
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,

  testCase "pNumFail2" $ case parseString "01" of
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pNumFail3" $ case parseString "-01" of
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,


--------------------------------------------------------------------pIdent--------------------------------------------------------------------
  testCase "pIdent1" $ parseString "x" 
    @?= Right [SExp (Var ("x"))],
  testCase "pIdent2" $ parseString "_x" 
    @?= Right [SExp (Var ("_x"))],
  testCase "pIdent3" $ parseString "_1" 
    @?= Right [SExp (Var ("_1"))],
  testCase "pIdent4" $ parseString "var" 
    @?= Right [SExp (Var ("var"))],
  testCase "pIdent5" $ parseString "var1_" 
    @?= Right [SExp (Var ("var1_"))],
  testCase "pIdent6" $ parseString "_var1_" 
    @?= Right [SExp (Var ("_var1_"))],
  testCase "pIdent7" $ parseString "_4var1_" 
    @?= Right [SExp (Var ("_4var1_"))],
  testCase "pIdent8" $ parseString "_4var1_" 
    @?= Right [SExp (Var ("_4var1_"))],


--------------------------------------------------------------------pIdentFail--------------------------------------------------------------------
  testCase "pIdent1" $ case parseString "2var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent2" $ case parseString "3_var1_" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent3" $ case parseString "1x" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent4" $ case parseString "for" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent5" $ case parseString "if" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent6" $ case parseString "in" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent7" $ case parseString "not" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent8" $ case parseString "1_" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent9" $ case parseString "!a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent10" $ case parseString "a\"" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent11" $ case parseString "%a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent12" $ case parseString "&a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent13" $ case parseString "/a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent14" $ case parseString "(a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pIdent15" $ case parseString ")a" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  

--------------------------------------------------------------------pExprOpt--------------------------------------------------------------------

  testCase "ExprOpt1" $ parseString "x < 5" 
    @?= Right [SExp (Oper Less (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt2" $ parseString "x > 5" 
    @?= Right [SExp (Oper Greater (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt3" $ parseString "x == 5" 
    @?= Right [SExp (Oper Eq (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt4" $ parseString "x >= 5" 
    @?= Right [SExp (Not (Oper Less (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt5" $ parseString "x <= 5" 
    @?= Right [SExp (Not (Oper Greater (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt6" $ parseString "x != 5" 
    @?= Right [SExp (Not (Oper Eq (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt7" $ parseString "x in 5" 
    @?= Right [SExp (Oper In (Var "x") (Const (IntVal 5)))],
  testCase "ExprOpt8" $ parseString "True == False" 
    @?= Right [SExp (Oper Eq (Const (TrueVal)) (Const (FalseVal)))],
  testCase "ExprOpt9" $ parseString "x not in 5" 
    @?= Right [SExp (Not (Oper In (Var "x") (Const (IntVal 5))))],
  testCase "ExprOpt9" $ parseString "True != False" 
    @?= Right [SExp (Not (Oper Eq (Const (TrueVal)) (Const (FalseVal))))],
               
  
  testCase "ExprOptFail1" $ 
    case parseString "1 < 2 < 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail2" $ 
    case parseString "1 > 2 > 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail3" $ 
    case parseString "1 == 2 == 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail4" $ 
    case parseString "1 >= 2 >= 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail5" $ 
    case parseString "1 <= 2 <= 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail6" $ 
    case parseString "1 in 2 in 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
  testCase "ExprOptFail7" $ 
    case parseString "1 != 2 != 3" of  
    Left e -> return ()
    Right p -> assertFailure $ "Unexpected parse: " ++ show p,

--------------------------------------------------------------------Not--------------------------------------------------------------------

  testCase "NUUT NUUT" $ parseString "not not x + 5" 
    @?= Right [SExp (Not (Not (Oper Plus (Var "x") (Const (IntVal 5)))))],
--------------------------------------------------------------------TermOpt--------------------------------------------------------------------

  testCase "TermOpt1" $ parseString "x + 5" 
    @?= Right [SExp (Oper Plus (Var "x") (Const (IntVal 5)))],
  testCase "TermOpt2" $ parseString "x - 5" 
    @?= Right [SExp (Oper Minus (Var "x") (Const (IntVal 5)))],
  testCase "TermOpt3" $ parseString "x + y" 
    @?= Right [SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "TermOpt4" $ parseString "x - y" 
    @?= Right [SExp (Oper Minus (Var "x") (Var "y"))],
  testCase "TermOpt5" $ parseString "0 - -5" 
    @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt6" $ parseString "0 + -5" 
    @?= Right [SExp (Oper Plus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOpt7" $ parseString "-0 - -5" 
    @?= Right [SExp (Oper Minus (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "TermOptFail1" $ case parseString "00 - -5" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "TermOptFail2" $ case parseString "5 - -001" of  
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,

--------------------------------------------------------------------FactorOpt--------------------------------------------------------------------

  testCase "FactorOpt1" $ parseString "10 * -5" 
    @?= Right [SExp (Oper Times (Const (IntVal 10)) (Const (IntVal (-5))))],
  testCase "FactorOpt2" $ parseString "-0 // -5" 
    @?= Right [SExp (Oper Div (Const (IntVal 0)) (Const (IntVal (-5))))],
  testCase "FactorOpt3" $ parseString "20 // 3" 
    @?= Right [SExp (Oper Div (Const (IntVal 20)) (Const (IntVal (3))))],
  testCase "FactorOpt4" $ parseString "20 // 0" 
    @?= Right [SExp (Oper Div (Const (IntVal 20)) (Const (IntVal (0))))],
  testCase "FactorOpt5" $ parseString "20 % 40" 
    @?= Right [SExp (Oper Mod (Const (IntVal 20)) (Const (IntVal (40))))],


--------------------------------------------------------------------FactorTermExprOpt--------------------------------------------------------------------

  testCase "FactorOpt&TermOpt&ExprOpt1" $ 
    parseString "x + y < x - y" 
    @?= Right [SExp (Oper Less (Oper Plus (Var "x") (Var "y")) 
              (Oper Minus (Var "x") (Var "y")))],
  testCase "FactorOpt&TermOpt&ExprOpt2" $ 
    parseString "x * y > x // y" 
    @?= Right [SExp (Oper Greater (Oper Times (Var "x") (Var "y")) 
              (Oper Div (Var "x") (Var "y")))],
  testCase "FactorOpt&TermOpt&ExprOpt3" $ 
  parseString "x + y <= x // y" 
  @?= Right [SExp (Not (Oper Greater (Oper Plus (Var "x") (Var "y")) 
            (Oper Div (Var "x") (Var "y"))))],
  testCase "FactorOpt&TermOpt&ExprOpt4" $ 
  parseString "x * y >= x - y" 
  @?= Right [SExp (Not (Oper Less (Oper Times (Var "x") (Var "y")) 
            (Oper Minus (Var "x") (Var "y"))))],


--------------------------------------------------------------------pString--------------------------------------------------------------------

  testCase "pString1" $ 
    parseString "'This is a string'" 
    @?= Right [SExp (Const (StringVal ("This is a string")))],
  testCase "pString2" $ 
    parseString "'This is a \n string'" 
    @?= Right [SExp (Const (StringVal ("This is a string")))],
  testCase "pString3" $ 
    parseString "'This is \t a string'" 
    @?= Right [SExp (Const (StringVal ("This is \t a string")))],
  testCase "pString4" $ 
    parseString "'This is \\ a string'" 
    @?= Right [SExp (Const (StringVal ("This is \\ a string")))],
  testCase "pString5" $ 
    parseString "'This is \\\' a string'" 
    @?= Right [SExp (Const (StringVal ("This is \' a string")))],
  testCase "pString6" $ 
    parseString "'This is a com#ment'" 
    @?= Right [SExp (Const (StringVal ("This is a com")))],
  testCase "pString7" $ 
    parseString "''" 
    @?= Right [SExp (Const (StringVal ("")))],

  testCase "pStringFail1" $ case parseString "'This is no#t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure 
                                      $ "Unexpected parse: " 
                                      ++ show p,
  testCase "pStringFail2" $ case parseString "'This is no#t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure 
                                      $ "Unexpected parse: " 
                                      ++ show p,
  testCase "pStringFail3" $ case parseString "'This is not \\\a string'" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pStringFail4" $ case parseString "'This is not \a string'" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pStringFail5" $ case parseString "'This is no§t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pStringFail6" $ case parseString "'This is no¾t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,
  testCase "pStringFail7" $ case parseString "'This is no½t a string" of 
                          Left e -> return ()
                          Right p -> assertFailure $ 
                                     "Unexpected parse: " 
                                     ++ show p,

--------------------------------------------------------------------Compr--------------------------------------------------------------------

  testCase "Compr1" $ 
    parseString "[x for x in x]" 
    @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x")])],
  testCase "Compr5" $ 
    parseString "[x for x in x if u]" 
      @?= Right [SExp (Compr (Var ("x")) [CCFor "x" (Var "x"), CCIf (Var "u")])],
  testCase "Compr6" $ 
    parseString "[x for x in x in u]" 
    @?= Right [SExp (Compr (Var "x") [CCFor "x" (Oper In (Var "x") (Var "u"))])],
  testCase "Compr7" $ 
    parseString "[x+2 for x in x for y in 7 if 1 < 2]" 
      @?= Right [SExp (Compr (Oper Plus (Var "x") (Const (IntVal 2))) 
                [CCFor "x" (Var "x"),CCFor "y" (Const (IntVal 7)),
                CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "ComprFail1" $ case parseString "[x for x in x ift]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "ComprFail2" $ case parseString "[xfor y in z]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "ComprFail3" $ case parseString "[x forz in k]" of 
                          Left e -> return ()
                          Right p -> assertFailure $ "Unexpected parse: " ++ show p,


--------------------------------------------------------------------pExprs--------------------------------------------------------------------

  testCase "pExprs1" $ 
    parseString "5; 4" 
    @?= Right [SExp (Const (IntVal 5)), SExp (Const (IntVal 4))],
  testCase "pExprs2" $ 
    parseString "5 + x; 4" 
    @?= Right [SExp (Oper Plus (Const (IntVal 5)) (Var "x")),
              SExp (Const (IntVal 4))],
  testCase "pExprs3" $ 
    parseString "'This is a string'; 4" 
    @?= Right [SExp (Const (StringVal "This is a string")),
              SExp (Const (IntVal 4))],
  testCase "pExprs4" $ 
    parseString "'This is a string'; 4; [x for x in x]" 
    @?= Right [SExp (Const (StringVal "This is a string")),
              SExp (Const (IntVal 4)), SExp (Compr (Var ("x")) 
              [CCFor "x" (Var "x")])],
  testCase "pExprs5" $ 
    parseString "'This is a string'; 4 % 8; [x for x in x]" 
    @?= Right [SExp (Const (StringVal "This is a string")),
              SExp (Oper Mod (Const (IntVal 4)) (Const (IntVal 8))),
              SExp (Compr (Var "x") [CCFor "x" (Var "x")])],
  testCase "pExprs6" $ 
    parseString "x=5; x+2" 
    @?= Right [SDef "x" (Const (IntVal 5)),
              SExp (Oper Plus (Var "x") (Const (IntVal 2)))],
  testCase "pExprs7" $ 
    parseString "x=5; y=7; x+y" 
    @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),
              SExp (Oper Plus (Var "x") (Var "y"))],
  testCase "pExprs8" $ 
    parseString "x=5; y=7; [x for x in y if 1 < 2]" 
      @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),
                SExp (Compr (Var "x") [CCFor "x" (Var "y"),
                CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "pExprs9" $ 
    parseString "x = 5; y = 7; [x for x in y if 1 < 2]" 
      @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" (Const (IntVal 7)),
                SExp (Compr (Var "x") [CCFor "x" (Var "y"),
                CCIf (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "pExprs10" $ 
    parseString "x = 5; y = 7*x; [x for x in y if 1 < 2]" 
      @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" 
                (Oper Times (Const (IntVal 7)) (Var "x")),
                SExp (Compr (Var "x") [CCFor "x" (Var "y"),CCIf 
                (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))])],
  testCase "pExprs11" $ 
    parseString "x = 5; y = 7; [x      for x    in y     if 1  < 2    ]" 
      @?= Right [SDef "x" (Const (IntVal 5)),SDef "y" 
                (Const (IntVal 7)),SExp (Compr (Var "x") 
                [CCFor "x" (Var "y"),CCIf (Oper Less (Const (IntVal 1)) 
                (Const (IntVal 2)))])],


--------------------------------------------------------------------pIdent pExprs--------------------------------------------------------------------
  testCase "ident (Exp)" $ parseString "range (5)" 
    @?= Right [SExp (Call "range" [Const (IntVal 5)])],
  testCase "ident (Exp)" $ parseString "range (5+y)" 
    @?= Right [SExp (Call "range" 
    [Oper Plus (Const (IntVal 5)) (Var "y")])],
  testCase "ident (Exp)" $ parseString "print (5)" 
    @?= Right [SExp (Call "print" [Const (IntVal 5)])],
  testCase "ident (Exp)" $ parseString "print (5 + 6)" 
    @?= Right [SExp (Call "print" 
    [Oper Plus (Const (IntVal 5)) (Const (IntVal 6))])],
  testCase "ident (Exp)" $ parseString "print ('This is a string')" 
    @?= Right [SExp (Call "print" [Const (StringVal "This is a string")])],
  testCase "ident (Exp)" $ parseString "print (x == Y)" 
    @?= Right [SExp (Call "print" [Oper Eq (Var "x") (Var "Y")])],
  testCase "finaltest" $ parseString "1" 
    @?= Right [SExp (Const (IntVal 1))]
  ]