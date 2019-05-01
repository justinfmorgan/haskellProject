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
                assertEqual "\\x -> x y" (Set.singleton (UndefinedVarUse ("unbound variable y"))) (check (Lam "x" (App (ValChar 'x') (ValChar 'y'))))
                assertEqual "\\xa -> xa ya" (Set.singleton (UndefinedVarUse ("unbound variable ya"))) (check(Lam "xa" (App (Var "xa") (Var "yb"))))
                assertEqual "\\xa -> 4 + xa * ya" (Set.singleton (UndefinedVarUse ("unbound variable ya"))) (check (Lam "xa" (Plus (Mult (ValInt 4) (Var "xa")) (Var "ya"))))
                assertEqual "\\xx -> \\yy -> \\zz -> xx - yy * zz + ww" (Set.singleton (UndefinedVarUse ("unbound variable ww"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (Plus (Minus (Var "xx") (Mult (Var "yy") (Var "zz"))) (Var "ww")))))),
  		
        testCase "UnusedVar" $
            do
                assertEqual "\\xx -> \\yy -> xx + 2" (Set.singleton (UnusedVar ("bound but unused variable yy"))) (check (Lam "xx" (Lam "yy" (Plus (Var "xx") (ValInt 2))))
                assertEqual "\\aa -> 2" (Set.singleton (UnusedVar ("bound but unused variable aa"))) (check (Lam "aa" (ValInt 2)))
                assertEqual "\\xx -> \\yy -> \\zz -> xx ^ yy" (Set.singleton (UnusedVar ("bound but unused variable zz"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (FloatExp (Var "xx") (Var "yy"))))))

        testCase "TypeError" $
            do
                assertEqual "a + a" (Set.singleton (TypeError "must add two floats or integers"))  (check (Plus (Var "aa") (Var "aa")) )
                assertEqual "True + True" (Set.singleton (TypeError "must add two floats or integers")) (check (Plus (Bool True) (Bool True)) )
                assertEqual "a - a" (Set.singleton (TypeError "must subtract two floats or integers")) (check (Minus (Var "aa") (Var "aa")) )
                assertEqual "True - True" (Set.singleton (TypeError "must subtract two floats or integers")) (check (Minus (Bool True) (Bool True)) )
                assertEqual "a * a" (Set.singleton (TypeError "must multiply two floats or integers")) (check (Mult (Var "aa") (Var "aa")) )
                assertEqual "True * True" (Set.singleton (TypeError "must multiply two floats or integers")) (check (Mult (Bool True) (Bool True)) )
                assertEqual "a / a" (Set.singleton (TypeError "must divide two integers")) (check (Div (Var "aa") (Var "aa")) )
                assertEqual "True / True" (Set.singleton (TypeError "must divide two integers")) (check (Div (Bool True) (Bool True)) )
                assertEqual "a < a" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThan (Var "aa") (Var "aa")) )
                assertEqual "True < True" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThan (Bool True) (Bool True)) )
                assertEqual "a <= a" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThanOrEqual (Var "aa") (Var "aa")) )
                assertEqual "True <= True" (Set.singleton (TypeError "must compare two floats or integers")) (check (LessThanOrEqual (Bool True) (Bool True)) )
                assertEqual "a > a" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreaterThan (Var "aa") (Var "aa")) )
                assertEqual "True > True" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreaterThan (Bool True) (Bool True)) )
                assertEqual "a >= a" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreatThanOrEqual (Var "aa") (Var "aa")) )
                assertEqual "True >= True" (Set.singleton (TypeError "must compare two floats or integers")) (check (GreatThanOrEqual (Bool True) (Bool True)) )
                assertEqual "a ** a" (Set.singleton (TypeError "must apply exponents to integers")) (check (IntExp (Var "aa") (Var "aa")) )
                assertEqual "True ** True" (Set.singleton (TypeError "must apply exponents to integers")) (check (IntExp (Bool True) (Bool True)) )
                assertEqual "a ^ a" (Set.singleton (TypeError "must apply exponents to integers")) (check (FloatExp (Var "aa") (Var "aa")) )
                assertEqual "True ^ True" (Set.singleton (TypeError "must apply exponents to floats")) (check (FloatExp (Bool True) (Bool True)) )
                assertEqual "a // a" (Set.singleton (TypeError "must divide floats")) (check (DivFloat (Var "aa") (Var "aa")) )
                assertEqual "True // True" (Set.singleton (TypeError "must divide floats")) (check (DivFloat (Bool True) (Bool True)) )
                assertEqual "a % a" (Set.singleton (TypeError "must module floats or integers")) (check (Modulus (Var "aa") (Var "aa")) )
                assertEqual "True % True" (Set.singleton (TypeError "must module floats or integers")) (check (Modulus (Bool True) (Bool True)) )


		testCase "No Error" $
			do
				assertEqual "\\x -> \\y -> x y" (Set.empty) (check (Lam "xx" (Lam "yy" (App (Var "xx") (Var "yy"))))
				assertEqual "\\x -> x" (Set.empty) (check (Lam "xx" (Var "xx"))
				assertEqual "\\x -> 2 + x" (Set.empty) (check (Lam "xx" (Plus (ValInt 2) (Var "xx")))
				assertEqual "\\x -> \\y -> \\z -> \\w -> x*y*z*w" (Set.empty) (check (Lam "xx" (Lam "yy" (Lam "zz" (Lam "ww" (Mult (Mult (Mult (Var "xx") (Var "yy")) (Var "zz")) (Var "ww"))))))
  ]

