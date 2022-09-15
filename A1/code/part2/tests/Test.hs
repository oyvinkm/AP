import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3, test4, test41, test5, test6, test7, test8, test9, test10, test101, test11, test111, test12, test13, test131] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))
  test4 = ("testLetErr", evalErr (Let {var = "x", def = Cst 5, body = Add (Let {var = "x", def = Add (Cst 3) (Cst 4), body = Mul (Var "x") (Var "x")}) (Var "x")}) initEnv == Right 54) 
  test41 = ("testLetFull", evalFull (Let {var = "x", def = Cst 5, body = Add (Let {var = "x", def = Add (Cst 3) (Cst 4), body = Mul (Var "x") (Var "x")}) (Var "x")}) initEnv == 54)
  test5 = ("testEBadVar", evalErr (Let {var = "x", def = Cst 5, body = Add (Let {var = "x", def = Add (Cst 3) (Cst 4), body = Mul (Var "y") (Var "x")}) (Var "x")}) initEnv == Left (EBadVar "y"))
  test6 = ("testNegPowErr", evalErr (Add (Pow (Cst 2) (Cst (-1))) (Div (Cst 2) (Cst 0))) initEnv == Left ENegPower)
  test7 = ("testEDivZero", evalErr (Add (Pow (Cst 2) (Cst (4))) (Div (Cst 2) (Cst 0))) initEnv == Left EDivZero)
  test8 = ("testAddPowDiv", evalErr (Add (Pow (Cst 2) (Cst (4))) (Div (Cst 8) (Cst 2))) initEnv == Right 20)
  test9 = ("testIfErr", evalErr (If (Sub (Add (Cst 2) (Cst 2)) (Mul (Cst 4) (Cst 1))) (Div (Cst 16) (Cst 4)) (Pow (Cst 2) (Cst (-4)))) initEnv == Left ENegPower)
  test10 = ("testIfElse", evalErr (If (Sub (Add (Cst 2) (Cst 2)) (Mul (Cst 4) (Cst 1))) (Div (Cst 16) (Cst 4)) (Pow (Cst 2) (Cst 3))) initEnv == Right 8)
  test101 = ("testIfElseFull", evalFull (If (Sub (Add (Cst 2) (Cst 2)) (Mul (Cst 4) (Cst 1))) (Div (Cst 16) (Cst 4)) (Pow (Cst 2) (Cst 3))) initEnv == 8)
  test11 = ("testSum", evalErr (Sum "x" (Cst (-4)) (Cst 8) (Add (Var "x") (Cst 2))) initEnv == Right 52)
  test111 = ("testSumFull", evalFull (Sum "x" (Cst (-4)) (Cst 8) (Add (Var "x") (Cst 2))) initEnv == 52)
  test12 = ("testSumErr", evalErr (Sum "x" (Cst (-4)) (Cst 8) (Add (Var "x") (Div (Cst 5) (Cst 0)))) initEnv == Left EDivZero)
  test13 = ("testIfThen", evalErr (If (Cst 1) (Cst 2) (Cst 3)) initEnv == Right 2)
  test131 = ("testIfThenFull", evalFull (If (Cst 1) (Cst 2) (Cst 3)) initEnv == 2)


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
