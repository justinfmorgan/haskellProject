module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval
import Exec
--import Examples (evalRes, Res(..))

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
twof = (ValFloat 2.0)
threef = (ValFloat 3.0)
nthreef = (ValFloat (-3.0))
onetwof = (ValFloat 1.2)
onefextra = (ValFloat 1.25)
nonefextra = (ValFloat (-1.25))
nonef = (ValFloat (-1.0))
fourfextra = (ValFloat 4.4)
nfourfextra = (ValFloat (-4.4))

true = (ValBool True)
false = (ValBool False)

char1 = (ValChar 'a')
char2 = (ValChar 'b')
char3 = (ValChar '3')
char4 = (ValChar '4')

simpleList1 = (Cons one Nil)
simpleList2 = (Cons two Nil)
simpleList3 = Nil
simpleList4 = (Cons ntwo Nil)
simpleList5 = (Cons nfourfextra Nil)
simpleList6 = (Cons true Nil)
simpleList7 = (Cons false Nil)

list1 = (Cons one (Cons two (Cons three (Cons four Nil))))
list2 = (Cons true (Cons false (Cons true (Cons false Nil))))
list3 = (Cons one (Cons onef (Cons true Nil)))
list4 = (Cons nonef (Cons onefextra (Cons nfourfextra (Cons nfour Nil))))

foldTestCase [] = return ()
foldTestCase (test1:testRest) = do test1
                                   foldTestCase testRest

evalTest = testGroup
      "Eval Test"
      [
         testCase "Basic Bools" $
            do
              assertEqual "T && T"      (Ok (B True) [])   (exec (And true true))
              assertEqual "T && F"      (Ok (B False) [])  (exec (And true false))
              assertEqual "F && F"      (Ok (B False) [])  (exec (And false false))
              assertEqual "F && T"      (Ok (B False) [])  (exec (And false true))
              assertEqual "T || F"      (Ok (B True) [])   (exec (Or true false))
              assertEqual "T || T"      (Ok (B True) [])   (exec (Or true true))
              assertEqual "F || F"      (Ok (B False) [])  (exec (Or false false))
              assertEqual "F || T"      (Ok (B True) [])   (exec (Or false true))
              assertEqual "!T"          (Ok (B False) [])  (exec (Not true))
              assertEqual "!F"          (Ok (B True) [])   (exec (Not false)),


         testCase "Compound Bools" $
            do
              assertEqual "T && (T && (!F))"      (Ok (B True) [])     (exec (And true (And true (Not false))))
              assertEqual "T || (F || (!T))"      (Ok (B True) [])     (exec (Or true (Or false (Not true))))
              assertEqual "F && (!T) && (!F)"     (Ok (B False) [])    (exec (And false (And (Not true) (Not false))))
              assertEqual "F || ((!T) && T)"      (Ok (B False) [])    (exec (Or false (And (Not true) true)))
              assertEqual "T && (F || T) && (!F) && (T &&  (!F))"
                                                  (Ok (B True) [])     (exec (And (And (And true (Or false true)) (Not false)) (And true (Not false)))),


         testCase "Basic Arithmetic" $
            do 
              assertEqual "2 + 4 =? "    (Ok (I 6) [])     (exec (Plus two four))
              assertEqual "2 + -1 =? "   (Ok (I 1) [])     (exec (Plus two none))
              assertEqual "2 - 4 =? "    (Ok (I (-2)) [])  (exec (Minus two four))
              assertEqual "2 - (-4) =? " (Ok (I 6) [])   (exec (Minus two nfour))
              assertEqual "3 * 2 =? "    (Ok (I 6) [])    (exec (Mult three two))
              assertEqual "1 // 2 =? "   (Ok (I 0) [])    (exec (Div one two))
              assertEqual "-2 // 2 =? "   (Ok (I (-1)) [])    (exec (Div ntwo two))
              assertEqual "1 // -1 =? "   (Ok (I (-1)) [])    (exec (Div one none))    
              assertEqual "4 // 2 =? "   (Ok (I 2) [])    (exec (Div four two))
              assertEqual "3 // (-1) =? "   (Ok (I (-3)) [])    (exec (Div three none))
              assertEqual "2 * -2 =? "   (Ok (I (-4)) []) (exec (Mult two ntwo))
              assertEqual "1 % 4 =? "    (Ok (I 1) []) (exec (Modulus one four))
              assertEqual "3 % 1 =?"      (Ok (I 0) []) (exec (Modulus three one))
              assertEqual "1.0 + 4.4 =? "    (Ok (F 5.4) [])    (exec (Plus onef fourfextra))
              assertEqual "-1.25 + 1.0 =? "   (Ok (F (-0.25)) [])    (exec (Plus nonefextra onef))
              assertEqual "1.0 - 4.4 =? "    (Ok (F (-3.4000000000000004)) []) (exec (Minus onef fourfextra))
              assertEqual "(-1.0) - (-4.4) =? " (Ok (F 3.4000000000000004) [])    (exec (Minus nonef nfourfextra))
              assertEqual "2.0 * 3.0 =? "    (Ok (F 6.0) [])   (exec (Mult twof threef))
              assertEqual "(1.0) / (-1.0) =? " (Ok (F (-1.0)) [])   (exec (DivFloat onef nonef))
              assertEqual "(-3.0) / (2.0) =? "    (Ok (F (-1.5)) [])    (exec (DivFloat nthreef twof)),

         testCase "Compound Arithmetic" $ ---TODO add compound with division and floats
            do 
              assertEqual "2 + 4 * 3  =? "             (Ok (I 14) [])  (exec (Plus two (Mult four three)))
              assertEqual "(2 + -4) * 3 =? "          (Ok (I (-6)) []) (exec (Mult (Plus two nfour) three))
              assertEqual "2 * 3 + 3 * 2 - 4 =? "     (Ok (I 8) [])   (exec (Minus (Plus (Mult two three) (Mult three two)) four))
              assertEqual "2 * (3 + 3) * (2 - 4) =? " (Ok (I (-24)) []) (exec (Mult (Mult two (Plus three three)) (Minus two four)))
              assertEqual "2 % (3 + 2)=?"             (Ok (I 2) []) (exec (Modulus two (Plus three two))),

         testCase "If Statements" $
            do 
              assertEqual "if 3 then 4 else 2 =? "       (Ok (I 4) [])  (exec (If three four two))
              assertEqual "if 0 then 1 else 4"           (Ok (I 4) [])  (exec (If zero one four))
              assertEqual "if 3 * 0 then 1 else 2  =? "  (Ok (I 2) [])  (exec (If (Mult three zero) one two))
              assertEqual "if 3 * 2 then 1 else 2  =? "  (Ok (I 1) [])  (exec (If (Mult three two) one two)),

         testCase "Let Statements" $
            do 
              assertEqual "let x = 4 in x * 2 =? "                   (Ok (I 8) [])  (exec (Let "x" four (Mult (Var "x") two)))
              assertEqual "let x = 4 * -2 in x - 2 =? "              (Ok (I (-10)) [])  (exec (Let "x" (Mult four ntwo) (Minus (Var "x") two)))
              assertEqual "let x = 2 in let y = x + 1 in y * 2 =? "  (Ok (I 6) [])  (exec (Let "x" two (Let "y" (Plus (Var "x") one)  (Mult (Var "y") two)))),

          testCase "Equal Statements" $
            do
              assertEqual "1 == 1 =? "   (Ok (B True) []) (exec (Equal one one))  --Integers
              assertEqual "-1 == -1 =? " (Ok (B True) []) (exec (Equal none none))
              assertEqual "-1 == -4 =? " (Ok (B False) []) (exec (Equal none nfour))
              assertEqual "-1 == 3 =? "  (Ok (B False) []) (exec (Equal none three))
              assertEqual "1 == -1 =? "  (Ok (B False) []) (exec (Equal one none))
              assertEqual "1 == -4 =? "  (Ok (B False) []) (exec (Equal one nfour))
              assertEqual "3 == 3 =? "   (Ok (B True) [])  (exec (Equal three three))
              assertEqual "1.0 == 1.0 =? "   (Ok (B True) []) (exec (Equal onef onef))  --Floats
              assertEqual "1.0 == 1.25 =? "  (Ok (B False) []) (exec (Equal onef onefextra)) 
              assertEqual "-1.0 == 1.0 =? "  (Ok (B False) []) (exec (Equal nonef onef)) 
              assertEqual "1.0 == -4.4 =? "  (Ok (B False) []) (exec (Equal onef nfourfextra)) 
              assertEqual "1.0 == -1.0 =? "  (Ok (B False) []) (exec (Equal onef nonef)) 
              assertEqual "1.25 == 1.25 =? " (Ok (B True) [])  (exec (Equal onefextra onefextra)) 
              assertEqual "-4.4 == -4.4 =? " (Ok (B True) [])  (exec (Equal nfourfextra nfourfextra))  
              assertEqual "True == True =? " (Ok (B True) []) (exec (Equal true true))
              assertEqual "True == False =? " (Ok (B False) []) (exec (Equal true false))
              assertEqual "False == True =? " (Ok (B False) []) (exec (Equal false true))
              assertEqual "False == False =? " (Ok (B True) []) (exec (Equal false false)),

          testCase "Not Equal Statements" $
            do
              assertEqual "1 /= 1 =? "   (Ok (B False) []) (exec (NotEqual one one))  --Integers
              assertEqual "-1 /= -1 =? " (Ok (B False) []) (exec (NotEqual none none))
              assertEqual "-1 /= -4 =? " (Ok (B True) []) (exec (NotEqual none nfour))
              assertEqual "-1 /= 3 =? "  (Ok (B True) []) (exec (NotEqual none three))
              assertEqual "1 /= -1 =? "  (Ok (B True) []) (exec (NotEqual one none))
              assertEqual "1 /= -4 =? "  (Ok (B True) []) (exec (NotEqual one nfour))
              assertEqual "3 /= 3 =? "   (Ok (B False) [])  (exec (NotEqual three three))
              assertEqual "1.0 /= 1.0 =? "   (Ok (B False) []) (exec (NotEqual onef onef))  --Floats
              assertEqual "1.0 /= 1.25 =? "  (Ok (B True) []) (exec (NotEqual onef onefextra)) 
              assertEqual "-1.0 /= 1.0 =? "  (Ok (B True) []) (exec (NotEqual nonef onef)) 
              assertEqual "1.0 /= -4.4 =? "  (Ok (B True) []) (exec (NotEqual onef nfourfextra)) 
              assertEqual "1.0 /= -1.0 =? "  (Ok (B True) []) (exec (NotEqual onef nonef)) 
              assertEqual "1.25 /= 1.25 =? " (Ok (B False) [])  (exec (NotEqual onefextra onefextra)) 
              assertEqual "-4.4 /= -4.4 =? " (Ok (B False) []) (exec (NotEqual nfourfextra nfourfextra))  
              assertEqual "True /= True =? " (Ok (B False) []) (exec (NotEqual true true))
              assertEqual "True /= False =? " (Ok (B True) []) (exec (NotEqual true false))
              assertEqual "False /= True =? " (Ok (B True) []) (exec (NotEqual false true))
              assertEqual "False /= False =?" (Ok (B False) []) (exec (NotEqual false false)),

          testCase "Greater Than Statements" $
            do
              assertEqual "1 > 1 =? "   (Ok (B False) []) (exec (GreaterThan one one))  --Integers
              assertEqual "-1 > -1 =? " (Ok (B False) []) (exec (GreaterThan none none))
              assertEqual "-1 > -4 =? " (Ok (B True) []) (exec (GreaterThan none nfour))
              assertEqual "-1 > 3 =? "  (Ok (B False) []) (exec (GreaterThan none three))
              assertEqual "1 > -1 =? "  (Ok (B True) []) (exec (GreaterThan one none))
              assertEqual "1 > -4 =? "  (Ok (B True) []) (exec (GreaterThan one nfour))
              assertEqual "3 > 3 =? "   (Ok (B False) [])  (exec (GreaterThan three three))
              assertEqual "1.0 > 1.0 =? "   (Ok (B False) []) (exec (GreaterThan onef onef))  --Floats
              assertEqual "1.0 > 1.25 =? "  (Ok (B False) []) (exec (GreaterThan onef onefextra)) 
              assertEqual "-1.0 > 1.0 =? "  (Ok (B False) []) (exec (GreaterThan nonef onef)) 
              assertEqual "1.0 > -4.4 =? "  (Ok (B True) []) (exec (GreaterThan onef nfourfextra)) 
              assertEqual "1.0 > -1.0 =? "  (Ok (B True) []) (exec (GreaterThan onef nonef)) 
              assertEqual "1.25 > 1.25 =? " (Ok (B False) [])  (exec (GreaterThan onefextra onefextra)) 
              assertEqual "-4.4 > -4.4 =? " (Ok (B False) [])  (exec (GreaterThan nfourfextra nfourfextra)),

          testCase "Greater Than Or Equal Statements" $
            do
              assertEqual "1 >= 1 =? "   (Ok (B True) []) (exec (GreatThanOrEqual one one))  --Integers
              assertEqual "-1 >= -1 =? " (Ok (B True) []) (exec (GreatThanOrEqual none none))
              assertEqual "-1 >= -4 =? " (Ok (B True) []) (exec (GreatThanOrEqual none nfour))
              assertEqual "-1 >= 3 =? "  (Ok (B False) []) (exec (GreatThanOrEqual none three))
              assertEqual "1 >= -1 =? "  (Ok (B True) []) (exec (GreatThanOrEqual one none))
              assertEqual "1 >= -4 =? "  (Ok (B True) []) (exec (GreatThanOrEqual one nfour))
              assertEqual "3 >= 3 =? "   (Ok (B True) [])  (exec (GreatThanOrEqual three three))
              assertEqual "1.0 >= 1.0 =? "   (Ok (B True) []) (exec (GreatThanOrEqual onef onef))  --Floats
              assertEqual "1.0 >= 1.25 =? "  (Ok (B False) []) (exec (GreatThanOrEqual onef onefextra)) 
              assertEqual "-1.0 >= 1.0 =? "  (Ok (B False) []) (exec (GreatThanOrEqual nonef onef)) 
              assertEqual "1.0 >= -4.4 =? "  (Ok (B True) []) (exec (GreatThanOrEqual onef nfourfextra)) 
              assertEqual "1.0 >= -1.0 =? "  (Ok (B True) []) (exec (GreatThanOrEqual onef nonef)) 
              assertEqual "1.25 >= 1.25 =? " (Ok (B True) [])  (exec (GreatThanOrEqual onefextra onefextra)) 
              assertEqual "-4.4 >= -4.4 =? " (Ok (B True) [])  (exec (GreatThanOrEqual nfourfextra nfourfextra)),

          testCase "Less Than Statements" $
            do
              assertEqual "1 < 1 =? "   (Ok (B False) []) (exec (LessThan one one))  --Integers
              assertEqual "-1 < -1 =? " (Ok (B False) []) (exec (LessThan none none))
              assertEqual "-1 < -4 =? " (Ok (B False) []) (exec (LessThan none nfour))
              assertEqual "-1 < 3 =? "  (Ok (B True) []) (exec (LessThan none three))
              assertEqual "1 < -1 =? "  (Ok (B False) []) (exec (LessThan one none))
              assertEqual "1 < -4 =? "  (Ok (B False) []) (exec (LessThan one nfour))
              assertEqual "3 < 3 =? "   (Ok (B False) [])  (exec (LessThan three three))
              assertEqual "1.0 < 1.0 =? "   (Ok (B False) []) (exec (LessThan onef onef))  --Floats
              assertEqual "1.0 < 1.25 =? "  (Ok (B True) []) (exec (LessThan onef onefextra)) 
              assertEqual "-1.0 < 1.0 =? "  (Ok (B True) []) (exec (LessThan nonef onef)) 
              assertEqual "1.0 < -4.4 =? "  (Ok (B False) []) (exec (LessThan onef nfourfextra)) 
              assertEqual "1.0 < -1.0 =? "  (Ok (B False) []) (exec (LessThan onef nonef)) 
              assertEqual "1.25 < 1.25 =? " (Ok (B False) [])  (exec (LessThan onefextra onefextra)) 
              assertEqual "-4.4 < -4.4 =? " (Ok (B False) [])  (exec (LessThan nfourfextra nfourfextra)),

          testCase "Less Than Or Equal Statements" $
            do
              assertEqual "1 <= 1 =? "   (Ok (B True) []) (exec (LessThanOrEqual one one))  --Integers
              assertEqual "-1 <= -1 =? " (Ok (B True) []) (exec (LessThanOrEqual none none))
              assertEqual "-1 <= -4 =? " (Ok (B False) []) (exec (LessThanOrEqual none nfour))
              assertEqual "-1 <= 3 =? "  (Ok (B True) []) (exec (LessThanOrEqual none three))
              assertEqual "1 <= -1 =? "  (Ok (B False) []) (exec (LessThanOrEqual one none))
              assertEqual "1 <= -4 =? "  (Ok (B False) []) (exec (LessThanOrEqual one nfour))
              assertEqual "3 <= 3 =? "   (Ok (B True) [])  (exec (LessThanOrEqual three three))
              assertEqual "1.0 <= 1.0 =? "   (Ok (B True) []) (exec (LessThanOrEqual onef onef))  --Floats
              assertEqual "1.0 <= 1.25 =? "  (Ok (B True) []) (exec (LessThanOrEqual onef onefextra)) 
              assertEqual "-1.0 <= 1.0 =? "  (Ok (B True) []) (exec (LessThanOrEqual nonef onef)) 
              assertEqual "1.0 <= -4.4 =? "  (Ok (B False) []) (exec (LessThanOrEqual onef nfourfextra)) 
              assertEqual "1.0 <= -1.0 =? "  (Ok (B False) []) (exec (LessThanOrEqual onef nonef)) 
              assertEqual "1.25 <= 1.25 =? " (Ok (B True) [])  (exec (LessThanOrEqual onefextra onefextra)) 
              assertEqual "-4.4 <= -4.4 =? " (Ok (B True) [])  (exec (LessThanOrEqual nfourfextra nfourfextra)),

          testCase "Concat Statements" $
            do
              assertEqual "[1] ++ [] =? " (Ok (Ls [I 1]) []) (exec (Concat simpleList1 Nil))
              assertEqual "[] ++ [] =? " (Ok (Ls []) [])    (exec (Concat Nil Nil))
              assertEqual "[1,2,3,4] ++ [1,2,3,4] =? " (Ok (Ls [I 1,I 2,I 3,I 4,I 1,I 2,I 3,I 4] ) [])   (exec (Concat list1 list1))
              assertEqual "[True,False,True,False] ++ [1] =? " (Ok (Ls [B True, B False, B True, B False, I 1] ) [])   (exec (Concat list2 simpleList1))
              assertEqual "[1,(1.0),True] ++ [(-4.4)] =? " (Ok (Ls [I 1,F(1.0),B True,F (-4.4)] ) [])   (exec (Concat list3 simpleList5))
              assertEqual "[False] ++ [1,2,3,4] =? " (Ok (Ls [B False, I 1, I 2, I 3, I 4] ) [])   (exec (Concat simpleList7 list1))
              assertEqual "[] ++ [1] =? " (Ok (Ls [I 1]) []) (exec (Concat Nil simpleList1)),

          testCase "Lists Statments" $
            do 
              assertEqual "Cons 1 Nil =?" (Ok (Ls [I 1])[]) (exec simpleList1)
              assertEqual "Cons 2 Nil =?" (Ok (Ls [I 2])[]) (exec simpleList2)
              assertEqual "Nil =?" (Ok (Ls [])[]) (exec simpleList3)          
              assertEqual "Cons (-2) Nil =?" (Ok (Ls [I (-2)])[]) (exec simpleList4) 
              assertEqual "Cons -4.4 Nil =?" (Ok (Ls [F (-4.4)])[]) (exec simpleList5)
              assertEqual "Cons true Nil =?" (Ok (Ls [B True])[]) (exec simpleList6)                               
              assertEqual "Cons 1 (Cons 1.0 (Cons True Nil)) =?" (Ok (Ls [I 1, F 1.0, B True]) []) (exec list3)
              assertEqual "Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))) =?" (Ok (Ls [I 1, I 2, I 3, I 4]) []) (exec list1)
              assertEqual "Cons true (Cons false (Cons True (Cons false Nil))) =?" (Ok (Ls [B True, B False, B True, B False]) []) (exec list2)
              assertEqual "Cons (-1.0) (Cons 1.25 (Cons (-4.4) (Cons (-4) Nil))) =?" (Ok (Ls [F (-1.0), F 1.25, F (-4.4), I (-4)]) []) (exec list4),
  
          testCase "Exponenents Statements" $
           do 
            assertEqual "2 ** 4 =?" (Ok (I 16) []) (exec (IntExp two four))
            assertEqual "4 ** 1 =?"  (Ok (I 4) []) (exec (IntExp four one))
            assertEqual "3.0 ^ 1.0 =?" (Ok (F 3.0) []) (exec (FloatExp threef onef))
            assertEqual "1.2 ^ 3.0 =?" (Ok (F (1.728)) []) (exec (FloatExp onetwof threef)),
            

          testCase "Separator Statements" $
          do
           assertEqual "(3 + 2);(2 + 1) = ?" (Ok (I 3) []) (exec (Separator (Plus three two) (Plus two one)))
           assertEqual "(4 - 1);(4 + (2 * 3)) =?" (Ok (I 10) []) (exec (Separator (Minus four one)(Plus four (Mult two three)))),
        
          
          testCase "ListIndex" $
            do
              assertEqual "[1] !! 0 =?" (Ok (I 1) []) (exec (ListIndex simpleList1 zero))
              assertEqual "[(-2)] !! 0 =?" (Ok (I (-2)) []) (exec (ListIndex simpleList4 zero)) 
              assertEqual "[True, False, True, False] !! 2 =?" (Ok (B True) []) (exec (ListIndex list2 two)) 
              assertEqual "[True, False, True, False] !! 3 =?" (Ok (B False) []) (exec (ListIndex list2 three))
              assertEqual "[1,2,3,4] !! 2 =?" (Ok (I 3) []) (exec (ListIndex list1 two)) 
              assertEqual "[1,2,3,4] !! 0 =?" (Ok (I 1) []) (exec (ListIndex list1 zero))
              assertEqual "[(-1.0),(1.25),(-4.4),(-4)] !! 2 =?" (Ok (F (-4.4)) []) (exec (ListIndex list4 two)) 
              assertEqual "[(-1.0),(1.25),(-4.4),(-4)] !! 3 =?" (Ok (I (-4)) []) (exec (ListIndex list4 three)),

          testCase "Var Lam App" $
            do
              assertEqual "(\\x -> x) 5 = ?" (Ok (I 5) []) (exec (App (Lam "x" (Var "x")) (ValInt 5)))
              assertEqual "(\\xx -> \\yy -> xx * yy - 22) 17 28 = ?" (Ok (I 454) []) 
                          (exec (App (App (Lam "xx" (Lam "yy" (Minus (Mult (Var "xx") (Var "yy")) (ValInt 22)))) (ValInt 17)) (ValInt 28)))
              assertEqual "(\\aa -> \\bb -> aa) true false = ?" (Ok (B True) []) (exec (App (App (Lam "aa" (Lam "bb" (Var "aa"))) (ValBool True)) (ValBool False)))
              assertEqual "(\\aa -> \\bb -> \\cc -> aa*bb*cc) 4 5 6 = ?" (Ok (I 120) [])
                          (exec (App (App (App (Lam "aa" (Lam "bb" (Lam "cc" (Mult (Mult (Var "aa") (Var "bb")) (Var "cc"))))) (ValInt 4)) (ValInt 5)) (ValInt 6)))
              assertEqual "(\\x -> x) 4.5 = ?" (Ok (F 4.5) []) (exec ((App (Lam "x" (Var "x")) (ValFloat 4.5))))
              assertEqual "(\\aa -> \\bb -> [aa,bb,5]) true 10 = ?" (Ok (Ls [B True, I 10, I 5]) []) 
                          (exec (App (App (Lam "aa" (Lam "bb" (Cons (Var "aa") (Cons (Var "bb") (Cons (ValInt 5) Nil))))) (ValBool True)) (ValInt 10))),

          testCase "Infix Func Compositions" $
            do
              assertEqual "(\\xx -> xx*5) . (\\yy -> yy*5) 10 = ?" (Ok (I 250) []) 
                          (exec (App (DotMixIn (Lam "xx" (Mult (Var "xx") (ValInt 5))) (Lam "yy" (Mult (Var "yy") (ValInt 5)))) (ValInt 10)))
              assertEqual "(\\xx -> xx) . (\\yy -> yy) 10 = ?" (Ok (I 10) [])
                          (exec (App (DotMixIn (Lam "xx" (Var "xx")) (Lam "yy" (Var "yy"))) (ValInt 10)))
              assertEqual "(\\bb -> bb^5) . (\\cc -> cc*3-19) 7.5 = ?" (Ok (F (525.21875)) [])
                          (exec (App (DotMixIn (Lam "bb" (FloatExp (Var "bb") (ValInt 5))) (Lam "cc" (Minus (Mult (Var "cc") (ValInt 3)) (ValInt 19)))) (ValFloat 7.5)))

    ]

