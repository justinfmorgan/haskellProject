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
                assertEqual "\\xa -> xa ya" 
                            (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ya"))) (check(Lam "xa" (App (Var "xa") (Var "ya"))))
                assertEqual "\\xa -> xa * ya + 4" 
                            (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ya"))) (check (Lam "xa" (Plus (Mult (Var "xa") (Var "ya")) (ValInt 4))))
                assertEqual "\\xx -> \\yy -> \\zz -> xx - yy * zz + ww" 
                            (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable ww"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (Plus (Minus (Var "xx") (Mult (Var "yy") (Var "zz"))) (Var "ww"))))))
                assertEqual "x + 2" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable x"))) (check (Plus (Var "x") (ValInt 2)))
                assertEqual "print(p); 1 + 2 + 3 * 4 ** 2" (Set.singleton (ErrorMsg ("Undefined variable use of an unbound variable p")))
                            (check (Separator (Print (Var "p")) (Plus (Plus (ValInt 1) (ValInt 2)) (Mult (ValInt 3) (IntExp (ValInt 4) (ValInt 2))))))
                assertEqual "undefined + 17 + turtle" (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable turtle", ErrorMsg "Undefined variable use of an unbound variable undefined"])
                            (check (Plus (Plus (Var "undefined") (ValInt 17)) (Var "turtle")))
                assertEqual "filtere (\\x -> x > o) (1 : 2 : banana : turtles : [])" 
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable banana",ErrorMsg "Undefined variable use of an unbound variable filtere",
                        ErrorMsg "Undefined variable use of an unbound variable o",ErrorMsg "Undefined variable use of an unbound variable turtles"])
                    (check (App (App (Var "filtere") (Lam "x" (GreaterThan (Var "x") (Var "o")))) 
                        (Cons (ValInt 1) (Cons (ValInt 2) (Cons (Var "banana") (Cons (Var "turtles") Nil))))))
                ,

        testCase "UnusedVar" $
            do
                assertEqual "\\xx -> \\yy -> xx + 2" (Set.singleton (UnusedVar ("bound but unused variable yy"))) (check (Lam "xx" (Lam "yy" (Plus (Var "xx") (ValInt 2)))))
                assertEqual "\\aa -> 2" (Set.singleton (UnusedVar ("bound but unused variable aa"))) (check (Lam "aa" (ValInt 2)))
                assertEqual "\\xx -> \\yy -> \\zz -> xx ^ yy" (Set.singleton (UnusedVar ("bound but unused variable zz"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (FloatExp (Var "xx") (Var "yy"))))))
                assertEqual "(\\x -> 3) (\\y -> \\z -> \\f -> f 17)" 
                    (Set.fromList [UnusedVar "bound but unused variable x",UnusedVar "bound but unused variable y",UnusedVar "bound but unused variable z"])
                    (check (App (Lam "x" (ValInt 3)) (Lam "y" (Lam "z" (Lam "f" (App (Var "f") (ValInt 17)))))))
                assertEqual "\\x -> \\y -> \\z -> print(x+y)" (Set.singleton (UnusedVar "bound but unused variable z"))
                    (check (Lam "x" (Lam "y" (Lam "z" (Print (Plus (Var "x") (Var "y")))))))
                assertEqual "\\turtle -> \\banana -> \\bananaTurtle -> (true && false)" 
                    (Set.fromList [UnusedVar "bound but unused variable banana",UnusedVar "bound but unused variable bananaTurtle",UnusedVar "bound but unused variable turtle"])
                    (check (Lam "turtle" (Lam "banana" (Lam "bananaTurtle" (And (ValBool True) (ValBool False))))))
                assertEqual "(\\x -> \\y -> true) (\\z -> false)" 
                    (Set.fromList [UnusedVar "bound but unused variable x",UnusedVar "bound but unused variable y",UnusedVar "bound but unused variable z"])
                    (check (App (Lam "x" (Lam "y" (ValBool True))) (Lam "z" (ValBool False))))
                ,
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
                assertEqual "\\x -> \\y -> 2" (Set.union (Set.singleton (UnusedVar ("bound but unused variable x") )) (Set.singleton (UnusedVar ("bound but unused variable y"))) ) (check (Lam "x" (Lam "y" (ValInt 2))))
                assertEqual "(\\x -> \\y -> turtle 2 TurtleBananasEveryWhere 7) (\\f -> f f f f e f f f f)"
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable TurtleBananasEveryWhere",
                        ErrorMsg "Undefined variable use of an unbound variable e",ErrorMsg "Undefined variable use of an unbound variable turtle",
                        UnusedVar "bound but unused variable x",UnusedVar "bound but unused variable y"])
                    (check (App (Lam "x" (Lam "y" (App (App (App (Var "turtle") (ValInt 2)) (Var "TurtleBananasEveryWhere")) (ValInt 7)))) (Lam "f" (App (App 
                        (App (App (App (App (App (App (Var "f") (Var "f")) (Var "f")) (Var "f")) (Var "e")) (Var "f")) (Var "f")) (Var "f")) (Var "f")))))
                assertEqual "\\oneError -> \\y -> twoError x z"
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable twoError",ErrorMsg "Undefined variable use of an unbound variable x",
                        ErrorMsg "Undefined variable use of an unbound variable z",UnusedVar "bound but unused variable oneError",UnusedVar "bound but unused variable y"])
                    (check (Lam "oneError" (Lam "y" (App (App (Var "twoError") (Var "x")) (Var "z")))))
                assertEqual "f f f f (\\toPayRespects -> z)"
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable f",ErrorMsg "Undefined variable use of an unbound variable z",
                        UnusedVar "bound but unused variable toPayRespects"])
                    (check (App (App (App (App (Var "f") (Var "f")) (Var "f")) (Var "f")) (Lam "toPayRespects" (Var "z"))))
                assertEqual "(\\x -> turtle) y"
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable turtle",ErrorMsg "Undefined variable use of an unbound variable y",UnusedVar "bound but unused variable x"])
                    (check (App (Lam "x" (Var "turtle")) (Var "y")))
                assertEqual "(\\a -> \\b -> c d e f g) 1 2 3 4 o 5"
                    (Set.fromList [ErrorMsg "Undefined variable use of an unbound variable c",ErrorMsg "Undefined variable use of an unbound variable d",
                        ErrorMsg "Undefined variable use of an unbound variable e",ErrorMsg "Undefined variable use of an unbound variable f",
                        ErrorMsg "Undefined variable use of an unbound variable g",ErrorMsg "Undefined variable use of an unbound variable o",
                        UnusedVar "bound but unused variable a",UnusedVar "bound but unused variable b"])
                    (check (App (App (App (App (App (App (Lam "a" (Lam "b" (App (App (App (App (Var "c") (Var "d")) (Var "e")) (Var "f")) (Var "g")))) 
                        (ValInt 1)) (ValInt 2)) (ValInt 3)) (ValInt 4)) (Var "o")) (ValInt 5)))
                ,

        testCase "No Error" $
            do
                assertEqual "\\x -> \\y -> x y" (Set.empty) (check (Lam "xx" (Lam "yy" (App (Var "xx") (Var "yy")))))
                assertEqual "\\x -> x" (Set.empty) (check (Lam "xx" (Var "xx")))
                assertEqual "\\x -> 2 + x" (Set.empty) (check (Lam "xx" (Plus (ValInt 2) (Var "xx"))))
                assertEqual "(\\aa -> \\bb -> \\cc -> aa*bb*cc) 4 5 6 = ?" (Set.empty) 
                    (check (App (App (App (Lam "aa" (Lam "bb" (Lam "cc" (Mult (Mult (Var "aa") (Var "bb")) (Var "cc"))))) (ValInt 4)) (ValInt 5)) (ValInt 6)))
                assertEqual "(\\x -> 1 + x) . (\\y -> y (\\z -> z > 1) y) (0 : 1 : 2 : 3 : [])" (Set.empty)
                    (check (App (DotMixIn (Lam "x" (Plus (ValInt 1) (Var "x"))) (Lam "y" (App (App (Var "y") (Lam "z" (GreaterThan (Var "z") (ValInt 1)))) 
                        (Var "y")))) (Cons (ValInt 0) (Cons (ValInt 1) (Cons (ValInt 2) (Cons (ValInt 3) Nil))))))
                assertEqual "\\x -> \\y -> \\z -> \\w -> x*y*z*w" (Set.empty) (check (Lam "xx" (Lam "yy" (Lam "zz" (Lam "ww" (Mult (Mult (Mult (Var "xx") (Var "yy")) (Var "zz")) (Var "ww")))))))
                assertEqual "(\\LastOne -> print(LastOne)) 58" (Set.empty)
                    (check (App (Lam "LastOne" (Print (Var "LastOne"))) (ValInt 58)))
--                assertEqual "\\x -> \\y -> 2 * 8 + x - y" (Set.empty) (check (Lam "x" (Lam "y" (Minus (Plus (Mult (ValInt 2) (ValInt 8)) (Var "x")) (Var "y")))))
           --     assertEqual "\\x -> \\y -> \\z -> \\a -> y + z - a + x" (Set.empty) (check (Lam "x" (Lam "y" (Lam "z" (Lam "a" (Plus (Minus (Plus (Var "y") (Var "z")) (Var "a")) (Var "x")))))))
          --      assertEqual "\\x -> 2:x" (Set.empty) (check (Lam "x" (Cons (ValInt 2) (Var "x"))) )
          --      assertEqual "\\x -> x ++ 8:9" (Set.empty) (check (Lam "x" (Concat (Var "x") (Cons (ValInt 8) (ValInt 9)))))
--                assertEqual "\\x -> !x" (Set.empty) (check (Lam "x" (Not (Var "x"))))
             --   assertEqual "\\x -> !x && x" (Set.empty) (check (Lam "x" (And (Not (Var "x")) (Var "x"))))
  ]

