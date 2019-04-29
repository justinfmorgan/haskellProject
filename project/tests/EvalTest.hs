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

zerof = (ValFloat 0.0)
onef = (ValFloat 1.0)
onefextra = (ValFloat 1.25)
nonefextra = (ValFloat (-1.25))
nonef = (ValFloat (-1.0))
fourfextra (ValFloat 4.4)
nfourfextra (ValFloat (-4.4))

true = (ValBool True)
false = (ValBool False)


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
              assertEqual "let x = 2 in let y = x + 1 in y * 2 =? "  6  (exec (Let "x" two (Let "y" (Plus (Var "x") one)  (Mult (Var "y") two)))),

          testCase "Equal Statements" $
            do
              assertEqual "1 == 1 =? "   True (exec (Equal one one))  --Integers
              assertEqual "-1 == -1 =? " True (exec (Equal none none))
              assertEqual "-1 == -4 =? " False (exec (Equal none nfour))
              assertEqual "-1 == 3 =? "  False (exec (Equal none three))
              assertEqual "1 == -1 =? "  False (exec (Equal one none))
              assertEqual "1 == -4 =? "  False (exec (Equal one nfour))
              assertEqual "3 == 3 =? "   True  (exec (Equal three three))
              assertEqual "1.0 == 1.0 =? "   True (exec (Equal onef onef))  --Floats
              assertEqual "1.0 == 1.25 =? "  False (exec (Equal onef onefextra)) 
              assertEqual "-1.0 == 1.0 =? "  False (exec (Equal nonef onef)) 
              assertEqual "1.0 == -4.4 =? "  False (exec (Equal onef nfourfextra)) 
              assertEqual "1.0 == -1.0 =? "  False (exec (Equal onef nonef)) 
              assertEqual "1.25 == 1.25 =? " True  (exec (Equal onefextra onefextra)) 
              assertEqual "-4.4 == -4.4 =? " True  (exec (Equal nfourfextra nfourfextra))  
              assertEqual "True == True =? " True (exec (Equal true true))
              assertEqual "True == False =? " False (exec (Equal true false))
              assertEqual "False == True =? " False (exec (Equal false true))
              assertEqual "False == False =? " True (exec (Equal false false)),

          testCase "Not Equal Statements" $
            do
              assertEqual "1 /= 1 =? "   False (exec (NotEqual one one))  --Integers
              assertEqual "-1 /= -1 =? " False (exec (NotEqual none none))
              assertEqual "-1 /= -4 =? " True (exec (NotEqual none nfour))
              assertEqual "-1 /= 3 =? "  True (exec (NotEqual none three))
              assertEqual "1 /= -1 =? "  True (exec (NotEqual one none))
              assertEqual "1 /= -4 =? "  True (exec (NotEqual one nfour))
              assertEqual "3 /= 3 =? "   False  (exec (NotEqual three three))
              assertEqual "1.0 /= 1.0 =? "   False (exec (NotEqual onef onef))  --Floats
              assertEqual "1.0 /= 1.25 =? "  True (exec (NotEqual onef onefextra)) 
              assertEqual "-1.0 /= 1.0 =? "  True (exec (NotEqual nonef onef)) 
              assertEqual "1.0 /= -4.4 =? "  True (exec (NotEqual onef nfourfextra)) 
              assertEqual "1.0 /= -1.0 =? "  True (exec (NotEqual onef nonef)) 
              assertEqual "1.25 /= 1.25 =? " False  (exec (NotEqual onefextra onefextra)) 
              assertEqual "-4.4 /= -4.4 =? " False (exec (NotEqual nfourfextra nfourfextra))  
              assertEqual "True /= True =? " False (exec (NotEqual true true))
              assertEqual "True /= False =? " True (exec (NotEqual true false))
              assertEqual "False /= True =? " True (exec (NotEqual false true))
              assertEqual "False /= False =?" False (exec (NotEqual false false)),

          testCase "Greater Than Statements"
            do
              assertEqual "1 > 1 =? "   False (exec (GreaterThan one one))  --Integers
              assertEqual "-1 > -1 =? " False (exec (GreaterThan none none))
              assertEqual "-1 > -4 =? " True (exec (GreaterThan none nfour))
              assertEqual "-1 > 3 =? "  False (exec (GreaterThan none three))
              assertEqual "1 > -1 =? "  True (exec (GreaterThan one none))
              assertEqual "1 > -4 =? "  True (exec (GreaterThan one nfour))
              assertEqual "3 > 3 =? "   False  (exec (GreaterThan three three))
              assertEqual "1.0 > 1.0 =? "   False (exec (GreaterThan onef onef))  --Floats
              assertEqual "1.0 > 1.25 =? "  False (exec (GreaterThan onef onefextra)) 
              assertEqual "-1.0 > 1.0 =? "  False (exec (GreaterThan nonef onef)) 
              assertEqual "1.0 > -4.4 =? "  True (exec (GreaterThan onef nfourfextra)) 
              assertEqual "1.0 > -1.0 =? "  True (exec (GreaterThan onef nonef)) 
              assertEqual "1.25 > 1.25 =? " False  (exec (GreaterThan onefextra onefextra)) 
              assertEqual "-4.4 > -4.4 =? " False  (exec (GreaterThan nfourfextra nfourfextra))  
              assertEqual "True > True =? " False (exec (GreaterThan true true))
              assertEqual "True > False =? " True (exec (GreaterThan true false))
              assertEqual "False > True =? " False (exec (GreaterThan false true))
              assertEqual "False > False =? " False (exec (GreaterThan false false)),

          testCase "Greater Than Or Equal Statements"
            do
              assertEqual "1 >= 1 =? "   True (exec (GreatThanOrEqual one one))  --Integers
              assertEqual "-1 >= -1 =? " False (exec (GreatThanOrEqual none none))
              assertEqual "-1 >= -4 =? " True (exec (GreatThanOrEqual none nfour))
              assertEqual "-1 >= 3 =? "  False (exec (GreatThanOrEqual none three))
              assertEqual "1 >= -1 =? "  True (exec (GreatThanOrEqual one none))
              assertEqual "1 >= -4 =? "  True (exec (GreatThanOrEqual one nfour))
              assertEqual "3 >= 3 =? "   True  (exec (GreatThanOrEqual three three))
              assertEqual "1.0 >= 1.0 =? "   True (exec (GreatThanOrEqual onef onef))  --Floats
              assertEqual "1.0 >= 1.25 =? "  False (exec (GreatThanOrEqual onef onefextra)) 
              assertEqual "-1.0 >= 1.0 =? "  True (exec (GreatThanOrEqual nonef onef)) 
              assertEqual "1.0 >= -4.4 =? "  True (exec (GreatThanOrEqual onef nfourfextra)) 
              assertEqual "1.0 >= -1.0 =? "  True (exec (GreatThanOrEqual onef nonef)) 
              assertEqual "1.25 >= 1.25 =? " True  (exec (GreatThanOrEqual onefextra onefextra)) 
              assertEqual "-4.4 >= -4.4 =? " True  (exec (GreatThanOrEqual nfourfextra nfourfextra))  
              assertEqual "True >= True =? " True (exec (GreatThanOrEqual true true))
              assertEqual "True >= False =? " True (exec (GreatThanOrEqual true false))
              assertEqual "False >= True =? " False (exec (GreatThanOrEqual false true))
              assertEqual "False >= False =? " True (exec (GreatThanOrEqual false false)),

          testCase "Less Than Statements"
            do
              assertEqual "1 < 1 =? "   False (exec (LessThan one one))  --Integers
              assertEqual "-1 < -1 =? " False (exec (LessThan none none))
              assertEqual "-1 < -4 =? " False (exec (LessThan none nfour))
              assertEqual "-1 < 3 =? "  True (exec (LessThan none three))
              assertEqual "1 < -1 =? "  False (exec (LessThan one none))
              assertEqual "1 < -4 =? "  False (exec (LessThan one nfour))
              assertEqual "3 < 3 =? "   False  (exec (LessThan three three))
              assertEqual "1.0 < 1.0 =? "   False (exec (LessThan onef onef))  --Floats
              assertEqual "1.0 < 1.25 =? "  True (exec (LessThan onef onefextra)) 
              assertEqual "-1.0 < 1.0 =? "  True (exec (LessThan nonef onef)) 
              assertEqual "1.0 < -4.4 =? "  False (exec (LessThan onef nfourfextra)) 
              assertEqual "1.0 < -1.0 =? "  False (exec (LessThan onef nonef)) 
              assertEqual "1.25 < 1.25 =? " False  (exec (LessThan onefextra onefextra)) 
              assertEqual "-4.4 < -4.4 =? " False  (exec (LessThan nfourfextra nfourfextra))  
              assertEqual "True < True =? " False (exec (LessThan true true))
              assertEqual "True < False =? " False (exec (LessThan true false))
              assertEqual "False < True =? " True (exec (LessThan false true))
              assertEqual "False < False =? " False (exec (LessThan false false)),

          testCase "Less Than Or Equal Statements"
            do
              assertEqual "1 <= 1 =? "   True (exec (LessThanOrEqual one one))  --Integers
              assertEqual "-1 <= -1 =? " True (exec (LessThanOrEqual none none))
              assertEqual "-1 <= -4 =? " False (exec (LessThanOrEqual none nfour))
              assertEqual "-1 <= 3 =? "  True (exec (LessThanOrEqual none three))
              assertEqual "1 <= -1 =? "  False (exec (LessThanOrEqual one none))
              assertEqual "1 <= -4 =? "  False (exec (LessThanOrEqual one nfour))
              assertEqual "3 <= 3 =? "   True  (exec (LessThanOrEqual three three))
              assertEqual "1.0 <= 1.0 =? "   True (exec (LessThanOrEqual onef onef))  --Floats
              assertEqual "1.0 <= 1.25 =? "  True (exec (LessThanOrEqual onef onefextra)) 
              assertEqual "-1.0 <= 1.0 =? "  True (exec (LessThanOrEqual nonef onef)) 
              assertEqual "1.0 <= -4.4 =? "  False (exec (LessThanOrEqual onef nfourfextra)) 
              assertEqual "1.0 <= -1.0 =? "  False (exec (LessThanOrEqual onef nonef)) 
              assertEqual "1.25 <= 1.25 =? " True  (exec (LessThanOrEqual onefextra onefextra)) 
              assertEqual "-4.4 <= -4.4 =? " True  (exec (LessThanOrEqual nfourfextra nfourfextra))  
              assertEqual "True <= True =? " True (exec (LessThanOrEqual true true))
              assertEqual "True <= False =? " False (exec (LessThanOrEqual true false))
              assertEqual "False <= True =? " True (exec (LessThanOrEqual false true))
              assertEqual "False <= False =? " True (exec (LessThanOrEqual false false))

    ]

