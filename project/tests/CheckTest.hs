module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 
import Data.Set (Set)
import qualified Data.Set as Set
import Ast
import Check

-- provide tests that show your check works

checktest = testGroup "CheckTest" 
  [
 -- error "no tests yet!"
        testCase "UndefinedVarUse" $
            do
                assertEqual "\\x -> x y" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable y"))) (check (Lam "x" (App (Var "x") (Var "y"))))
                assertEqual "\\xa -> xa ya" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ya"))) (check(Lam "xa" (App (Var "xa") (Var "ya"))))
                assertEqual "\\xa -> xa * ya + 4" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ya"))) (check (Lam "xa" (Plus (Mult (Var "xa") (Var "ya")) (ValInt 4))))
                assertEqual "\\xx -> \\yy -> \\zz -> xx - yy * zz + ww" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ww"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (Plus (Minus (Var "xx") (Mult (Var "yy") (Var "zz"))) (Var "ww")))))),
  		
        testCase "UnusedVar" $
            do
                assertEqual "\\xx -> \\yy -> xx + 2" (Set.singleton (UnusedVar ("bound but unused variable yy"))) (check (Lam "xx" (Lam "yy" (Plus (Var "xx") (ValInt 2)))))
                assertEqual "\\aa -> 2" (Set.singleton (UnusedVar ("bound but unused variable aa"))) (check (Lam "aa" (ValInt 2)))
                assertEqual "\\xx -> \\yy -> \\zz -> xx ^ yy" (Set.singleton (UnusedVar ("bound but unused variable zz"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (FloatExp (Var "xx") (Var "yy")))))),
{-
        testCase "TypeError" $
            do
                assertEqual "a + a" (Set.singleton (TypeError "must add two floats or integers"))  (check (Plus (Var "aa") (Var "aa")) )
                assertEqual "True + True" (Set.singleton (TypeError "must add two floats or integers")) (check (Plus (ValBool True) (ValBool True)) )
                assertEqual "a - a" (Set.singleton (TypeError "must subtract two floats or integers")) (check (Minus (Var "aa") (Var "aa")) )
                assertEqual "True - True" (Set.singleton (TypeError "must subtract two floats or integers")) (check (Minus (ValBool True) (ValBool True)) )
                assertEqual "a * a" (Set.singleton (TypeError "must multiply two floats or integers")) (check (Mult (Var "aa") (Var "aa")) )
                assertEqual "True * True" (Set.singleton (TypeError "must multiply two floats or integers")) (check (Mult (ValBool True) (ValBool True)) )
                assertEqual "a / a" (Set.singleton (TypeError "must divide two integers")) (check (Div (Var "aa") (Var "aa")) )
                assertEqual "True / True" (Set.singleton (TypeError "must divide two integers")) (check (Div (ValBool True) (ValBool True)) )
                assertEqual "a < a" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThan (Var "aa") (Var "aa")) )
                assertEqual "True < True" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThan (ValBool True) (ValBool True)) )
                assertEqual "a <= a" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThanOrEqual (Var "aa") (Var "aa")) )
                assertEqual "True <= True" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThanOrEqual (ValBool True) (ValBool True)) )
                assertEqual "a > a" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreaterThan (Var "aa") (Var "aa")) )
                assertEqual "True > True" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreaterThan (ValBool True) (ValBool True)) )
                assertEqual "a >= a" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreatThanOrEqual (Var "aa") (Var "aa")) )
                assertEqual "True >= True" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreatThanOrEqual (ValBool True) (ValBool True)) )
                assertEqual "a ** a" (Set.singleton (TypeError "must apply exponents to integers")) (check (IntExp (Var "aa") (Var "aa")) )
                assertEqual "True ** True" (Set.singleton (TypeError "must apply exponents to integers")) (check (IntExp (ValBool True) (ValBool True)) )
                assertEqual "a ^ a" (Set.singleton (TypeError "must apply exponents to integers")) (check (FloatExp (Var "aa") (Var "aa")) )
                assertEqual "True ^ True" (Set.singleton (TypeError "must apply exponents to floats")) (check (FloatExp (ValBool True) (ValBool True)) )
                assertEqual "a // a" (Set.singleton (TypeError "must divide floats")) (check (DivFloat (Var "aa") (Var "aa")) )
                assertEqual "True // True" (Set.singleton (TypeError "must divide floats")) (check (DivFloat (ValBool True) (ValBool True)) )
                assertEqual "a % a" (Set.singleton (TypeError "must module floats or integers")) (check (Modulus (Var "aa") (Var "aa")) )
                assertEqual "True % True" (Set.singleton (TypeError "must module floats or integers")) (check (Modulus (ValBool True) (ValBool True))),
-}
        testCase "Multiple Errors" $
            do
                assertEqual "\\x -> b" (Set.union (Set.singleton(ErrorMsg ("Undefined variable use of an unbound variable b"))) (Set.singleton(UnusedVar ("bound but unused variable x"))) ) (check (Lam "x" (Var "b")) )
                assertEqual "\\x -> \\y -> 2" (Set.union (Set.singleton (UnusedVar ("bound but unused variable x") )) (Set.singleton (UnusedVar ("bound but unused variable y"))) ) (check (Lam "x" (Lam "y" (ValInt 2)))),

		testCase "No Error" $
			do
				assertEqual "\\x -> \\y -> x y" (Set.empty) (check (Lam "xx" (Lam "yy" (App (Var "xx") (Var "yy")))))
				assertEqual "\\x -> x" (Set.empty) (check (Lam "xx" (Var "xx")))
           --     assertEqual "2+ \\x -> \\y -> x + y" (Set.empty) (check  (Plus (ValInt 2) (Lam "x" (Lam "y" (Plus (Var "x") (Var "y"))))) )
				assertEqual "\\x -> 2 + x" (Set.empty) (check (Lam "xx" (Plus (ValInt 2) (Var "xx"))))
				assertEqual "\\x -> \\y -> \\z -> \\w -> x*y*z*w" (Set.empty) (check (Lam "xx" (Lam "yy" (Lam "zz" (Lam "ww" (Mult (Mult (Mult (Var "xx") (Var "yy")) (Var "zz")) (Var "ww")))))))
--                assertEqual "\\x -> \\y -> 2 * 8 + x - y" (Set.empty) (check (Lam "x" (Lam "y" (Minus (Plus (Mult (ValInt 2) (ValInt 8)) (Var "x")) (Var "y")))))
           --     assertEqual "\\x -> \\y -> \\z -> \\a -> y + z - a + x" (Set.empty) (check (Lam "x" (Lam "y" (Lam "z" (Lam "a" (Plus (Minus (Plus (Var "y") (Var "z")) (Var "a")) (Var "x")))))))
          --      assertEqual "\\x -> 2:x" (Set.empty) (check (Lam "x" (Cons (ValInt 2) (Var "x"))) )
          --      assertEqual "\\x -> x ++ 8:9" (Set.empty) (check (Lam "x" (Concat (Var "x") (Cons (ValInt 8) (ValInt 9)))))
--                assertEqual "\\x -> !x" (Set.empty) (check (Lam "x" (Not (Var "x"))))
             --   assertEqual "\\x -> !x && x" (Set.empty) (check (Lam "x" (And (Not (Var "x")) (Var "x"))))
  ]

