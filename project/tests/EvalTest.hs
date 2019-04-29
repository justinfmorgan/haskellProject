module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval

-- provide tests that show your run/eval works

zero = (ValInt 0)
one = (ValInt 1)
none = (ValInt (-1))
two = (ValInt 2)
ntwo = (ValInt (-2))
three = (ValInt 3)
nthree = (ValInt (-3))
four = (ValInt 4)
nfour = (ValInt (-4))

true = (Valbool True)
false = (Valbool False)


evalTest = testGroup
      "Eval Test"
      [
         testCase "Basic Bools" $
            do
              assertEqual "T && T"      True    (exec (And true true))
              assertEqual "T && F"      False   (exec (And true false))
              assertEqual "F && F"      False   (exec (And false false))
              assertEqual "T || F"      True    (exec (Or true false))
              assertEqual "T || T"      True    (exec (Or true true))
              assertEqual "F || F"      False   (exec (Or false false))
              assertEqual "!T"          False   (exec (Not true))
              assertEqual "!F"          True    (exec (Not false)),


         testCase "Compound Bools" $
            do
              assertEqual "T && T && (!False)"      True    (exec (And true true))
              assertEqual "T && F"                  False   (exec (And true false))
              assertEqual "F && F"      False   (exec (And false false))
              assertEqual "T || F"      True    (exec (Or true false))
              assertEqual "T || T"      True    (exec (Or true true))
              assertEqual "F || F"      False   (exec (Or false false))
              assertEqual "!T"          False   (exec (Not true))
              assertEqual "!F"          True    (exec (Not false)), 


         testCase "Basic Arithmetic" $
            do 
              assertEqual "2 + 4 =? "    6    (exec (Plus two four))
              assertEqual "2 + -1 =? "   1    (exec (Plus two none))
              assertEqual "2 - 4 =? "    (-2) (exec (Sub two four))
              assertEqual "2 - (-4) =? " 6    (exec (Sub two nfour))
              assertEqual "3 * 2 =? "    6    (exec (Mult three two))
              assertEqual "2 * -2 =? "   (-4) (exec (Mult two ntwo)),

         testCase "Compound Arithmetic" $
            do 
              assertEqual "2 + 4 * 3 =? "             14   (exec (Plus two (Mult four three)))
              assertEqual "(2 + -4) * 3 =? "          (-6) (exec (Mult (Plus two nfour) three))
              assertEqual "2 * 3 + 3 * 2 - 4 =? "     8    (exec (Sub (Plus (Mult two three) (Mult three two)) four))
              assertEqual "2 * (3 + 3) * (2 - 4) =? " (-24) (exec (Mult (Mult two (Plus three three)) (Sub two four))),

         testCase "If Statements" $
            do 
              assertEqual "if 3 then 4 else 2 =? "       4  (exec (If three four two))
              assertEqual "if 0 then 1 else 4"           4  (exec (If zero one four))
              assertEqual "if 3 * 0 then 1 else 2  =? "  2  (exec (If (Mult three zero) one two))
              assertEqual "if 3 * 2 then 1 else 2  =? "  1  (exec (If (Mult three two) one two)),

         testCase "Let Statements" $
            do 
              assertEqual "let x = 4 in x * 2 =? "                   8  (exec (Let "x" four (Mult (Var "x") two)))
              assertEqual "let x = 4 * -2 in x - 2 =? "              (-10)  (exec (Let "x" (Mult four ntwo) (Sub (Var "x") two)))
              assertEqual "let x = 2 in let y = x + 1 in y * 2 =? "  6  (exec (Let "x" two (Let "y" (Plus (Var "x") one)  (Mult (Var "y") two))))
    ]

