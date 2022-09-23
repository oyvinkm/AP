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
  [testCase "CRASH TEST" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),
--------------------------------------------------------------------PRIMITIVES------------------------------------------------------------
    testCase "PRIMITIVES 1 ABORT" $ runComp (abort $ EBadVar "x" :: Comp Value) [] @?= (Left $ EBadVar "x", []),
    testCase "PRIMITIVES 2 LOOK" $ runComp (look "x") [("x", IntVal 2), ("y", IntVal 3)] @?= (Right $ IntVal 2, []),
    testCase "PRIMITIVES 3 LOOK" $ runComp (look "y") [("x", IntVal 2), ("y", IntVal 3)] @?= (Right $ IntVal 3, []),
    testCase "PRIMITIVES 4 WITHBINDING" $ runComp (withBinding "x" (IntVal 4) $ eval (Var "x")) [] @?= (Right $ IntVal 4, []),
    testCase "PRIMITIVES 5 OUTPUT" $ runComp (output "A string that's never printed, but still output(?)") [] 
                                    @?= (Right(), ["A string that's never printed, but still output(?)"]),
--------------------------------------------------------------------TRUTHY---------------------------------------------------------------
    testCase "THRUTHY 1" $ truthy  (IntVal 0) @?= False,
    testCase "THRUTHY 2" $ truthy NoneVal @?= False,
    testCase "THRUTHY 3" $ truthy (StringVal "") @?= False,
    testCase "THRUTHY 4" $ truthy FalseVal @?= False,
    testCase "THRUTHY 5" $ truthy (ListVal []) @?= False,
    testCase "THRUTHY 6" $ truthy (IntVal 3) @?= True,
    testCase "THRUTHY 7" $ truthy (ListVal [IntVal 3]) @?= True,
    testCase "CHECKLEN 1" $ checkLen [IntVal(4), IntVal(4), IntVal(4), IntVal(4)] @?= False,
    testCase "CHECKLEN 2" $ checkLen [IntVal(4), IntVal(4)] @?= True,
    testCase "CHECKLEN 2" $ checkLen [] @?= False,
    testCase "ISINTLIST 1" $ isIntList [IntVal(4), IntVal(4), IntVal(4), IntVal(4)] @?= True,
    testCase "ISINTLIST 2" $ isIntList [StringVal "A", StringVal "B"] @?= False,
    testCase "CONCAT STRINGS 1" $ concatStrings [] @?= "",

--------------------------------------------------------------------OPERATE---------------------------------------------------------------
    testCase "OPERATE 1 PLUS" $ operate Plus (IntVal 2) (IntVal 1) 
                                    @?= Right (IntVal 3),
    testCase "OPERATE 2 PLUS" $ operate Plus (IntVal (-2)) (IntVal 1) 
                                    @?= Right (IntVal (-1)),
    testCase "OPERATE 3 PLUS" $ operate Plus (IntVal 5) (StringVal "2") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 4 PLUS" $ operate Plus (IntVal 0) (IntVal 0) 
                                    @?= Right (IntVal 0),
    testCase "OPERATE 5 PLUS" $ operate Plus (StringVal "5") (IntVal 2) 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 6 PLUS" $ operate Plus (StringVal "5") (StringVal "5") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 7 MINUS" $ operate Minus (IntVal 0) (IntVal 0)
                                    @?= Right (IntVal 0),
    testCase "OPERATE 8 MINUS" $ operate Minus (IntVal 3) (IntVal (-5)) 
                                    @?= Right (IntVal 8),
    testCase "OPERATE 9 MINUS" $ operate Minus (StringVal "5") (IntVal 2) 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 10 MINUS" $ operate Minus (IntVal 2) (StringVal "5") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 11 MINUS" $ operate Minus (StringVal "5") (StringVal "5") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 12 MINUS" $ operate Minus (IntVal 3) (IntVal (5)) 
                                    @?= Right (IntVal (-2)),
    testCase "OPERATE 13 MINUS" $ operate Minus (IntVal (-3)) (IntVal (-1)) 
                                    @?= Right (IntVal (-2)),
    testCase "OPERATE 14 TIMES" $ operate Times (IntVal 14) (IntVal 16) 
                                    @?= Right (IntVal 224),
    testCase "OPERATE 15 TIMES" $ operate Times (IntVal (-17)) (IntVal 26) 
                                    @?= Right (IntVal (-442)),
    testCase "OPERATE 16 TIMES" $ operate Times (IntVal (-17)) (IntVal (-13)) 
                                    @?= Right (IntVal (221)),
    testCase "OPERATE 17 TIMES" $ operate Times (IntVal 2509) (IntVal 0) 
                                    @?= Right (IntVal 0),
    testCase "OPERATE 18 TIMES" $ operate Times (StringVal "5") (IntVal 2) 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 19 TIMES" $ operate Times (IntVal 2) (StringVal "5") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 20 TIMES" $ operate Times (StringVal "5") (StringVal "5") 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 21 DIV" $ operate Div (IntVal 14) (IntVal 7) 
                                    @?= Right (IntVal 2),
    testCase "OPERATE 22 DIV" $ operate Div (IntVal 334) (IntVal (-43)) 
                                    @?= Right (IntVal (-8)),
    testCase "OPERATE 23 DIV" $ operate Div (IntVal 23423) (IntVal 0) 
                                    @?= Left "Cannot divide by zero",
    testCase "OPERATE 24 DIV" $ operate Div (IntVal (-45)) (IntVal 7) 
                                    @?= Right (IntVal (-7)),
    testCase "OPERATE 25 MOD" $ operate Mod (IntVal (-343)) (IntVal 7) 
                                    @?= Right (IntVal 0), 
    testCase "OPERATE 26 MOD" $ operate Mod (IntVal 512) (IntVal 0) 
                                    @?= Left "Cannot do mod by zero",
    testCase "OPERATE 28 EQ" $ operate Eq (StringVal "Medici") (StringVal "Medici") 
                                    @?= Right TrueVal,
    testCase "OPERATE 29 EQ" $ operate Eq (StringVal "Medici") (StringVal "Medicii") 
                                    @?= Right FalseVal,
    testCase "OPERATE 30 EQ" $ operate Eq (IntVal 13) (IntVal 13) 
                                    @?= Right TrueVal,
    testCase "OPERATE 31 EQ" $ operate Eq (IntVal 13) (IntVal (-13)) 
                                    @?= Right FalseVal,
    testCase "OPERATE 32 LESS" $ operate Less (IntVal 0) (IntVal 0) 
                                    @?= Right FalseVal,
    testCase "OPERATE 33 LESS" $ operate Less (IntVal 1255) (IntVal 1590) 
                                    @?= Right TrueVal,
    testCase "OPERATE 34 LESS" $ operate Less (IntVal 1255) (IntVal (-1590)) 
                                    @?= Right FalseVal,
    testCase "OPERATE 35 GREATER" $ operate Greater (IntVal 0) (IntVal 0) 
                                    @?= Right FalseVal,
    testCase "OPERATE 36 GREATER" $ operate Greater (IntVal 1255) (IntVal (-1590)) 
                                    @?= Right TrueVal,
    testCase "OPERATE 37 GREATER" $ operate Greater (IntVal 1255) (IntVal (1590)) 
                                    @?= Right FalseVal,
    testCase "OPERATE 38 GREATER" $ operate Greater 
                                    (StringVal "1255") (IntVal (1590)) 
                                    @?= Left "Error, the operator couldn't handle the arguments.",
    testCase "OPERATE 39 IN" $ operate In (IntVal 2) 
                              (ListVal [(IntVal 1), (IntVal 2), (IntVal 3)]) 
                              @?= Right TrueVal,
    testCase "OPERATE 40 IN" $ operate In (IntVal 2) 
                              (ListVal [(IntVal 1), (IntVal 1), (IntVal 3)]) 
                              @?= Right FalseVal,
    testCase "OPERATE 41 IN" $ operate In (StringVal "A") 
                               (ListVal [(StringVal "A"), (StringVal "B"), (StringVal "C")]) 
                               @?= Right TrueVal,
    testCase "OPERATE 43 IN" $ operate In (StringVal "A") (ListVal [(StringVal "A"), (StringVal "B"), (StringVal "")]) @?= Right TrueVal,
    testCase "OPERATE 44 LEFT" $ operate Plus (StringVal "D") (IntVal 4) @?= Left "Error, the operator couldn't handle the arguments.",
--------------------------------------------------------------------APPLY---------------------------------------------------------------------
    testCase "APPLY 1 RANGE" $ runComp (apply "range" [IntVal 5]) [] @?= 
                                       (Right (ListVal [IntVal 0,IntVal 1, 
                                       IntVal 2,IntVal 3,IntVal 4]),[]),
    testCase "APPLY 2 RANGE" $ runComp (apply "range" [IntVal 3, IntVal 10]) [] @?= 
                                      (Right (ListVal [IntVal 3,IntVal 4,IntVal 5,
                                      IntVal 6,IntVal 7, IntVal 8, IntVal 9]), []),
    testCase "APPLY 3 RANGE" $ runComp (apply "range" [IntVal 3, IntVal 14, IntVal 2]) [] @?= 
                                      (Right (ListVal [IntVal 3,IntVal 5,IntVal 7,
                                      IntVal 9,IntVal 11, IntVal 13]), []),
    testCase "APPLY 4 RANGE" $ runComp (apply "range" [StringVal "4", StringVal "5"]) [] @?= (Left (EBadArg "Argument is invalid."),[]),
    testCase "APPLY 5 PRINT" $ runComp (apply "print" ([StringVal "This is a print statement", 
                                                        IntVal 44, ListVal [IntVal 2, IntVal 4], 
                                                        NoneVal])) [] 
                                                        @?= (Right NoneVal, 
                                                        ["This is a print statement 44 [2, 4] None"]),
    testCase "APPLY 6 FAIL" $ runComp (apply "failure" [NoneVal]) [] @?= (Left (EBadFun "failure"), []),
--------------------------------------------------------------------EVAL---------------------------------------------------------------------
    testCase "EVAL 1 CONST" $ runComp (eval (Const (IntVal 2))) [] @?= (Right (IntVal 2), []),
    testCase "EVAL 2 VAR" $ runComp (eval (Var "x")) [] @?= (Left (EBadVar "x"), []),
    testCase "EVAL 3 CALL" $ runComp (eval (Call "print" [Const (StringVal "This is a print statement"), 
                                                          Const (IntVal 44), Const (ListVal [IntVal 2, IntVal 4]), 
                                                          Const NoneVal])) [] 
                                                          @?= (Right NoneVal, ["This is a print statement 44 [2, 4] None"]),
    testCase "EVAL 4 OPER" $ runComp (eval (Oper Div (Const (IntVal 14))  (Const (IntVal 7)))) [] 
                                      @?= (Right (IntVal 2), []),
    testCase "EVAL 5 LIST" $ runComp (eval (List [Const (IntVal 44), Var "x", (Oper Div (Const (IntVal 14))  
                                      (Const (IntVal 7)))])) [("x", IntVal 5)] 
                                      @?= (Right (ListVal [IntVal 44, IntVal 5, IntVal 2]), []),
    testCase "EVAL 6 COMPR" $ runComp (eval (Compr (Oper Plus (Var "x") (Const (IntVal 2))) 
                                      ([(CCFor "x" (Const (ListVal[IntVal 2, IntVal 3])))]))) [("x", IntVal 2)] 
                                      @?= (Right (ListVal [IntVal 4, IntVal 5]), []),
    testCase "EVAL 7 COMPR" $ runComp (eval (Compr (Oper Plus (Var "x") (Const (IntVal 2))) 
                                      ([(CCFor "x" (Const (ListVal[IntVal 2, IntVal 3, IntVal 4]))),
                                      CCIf (Oper Greater (Var "x") (Const (IntVal 2)))]))) [] 
                                      @?= (Right (ListVal [IntVal 5, IntVal 6]), [])

  ]
    



--  Left "Cannot divide by zero"
--  Left "Cannot do mod by zero"
--  Left "Error, the operator couldn't handle the arguments."

-- execute [SExp (Call "print" [Oper Plus (Const (IntVal 2)) (Const (IntVal 2))]), SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 2)))]