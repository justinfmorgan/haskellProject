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
                assertEqual "\\xx -> \\yy -> \\zz -> xx - yy * zz + ww" (Set.singleton (UndefinedVarUse ("unbound variable ww"))) (check (Lam "xx" (Lam "yy" (Lam "zz" (Plus (Minus (Var "xx") (Mult (Var "yy") (Var "zz"))) (Var "ww"))))))
  				{-
		testCase "No Error" $
			do
				assertEqual "\x -> \y -> x y"
				assertEqual "\x -> x"
				assertEqual "\x -> 2 + x"
				assertEqual "\x -> 8 * x"		-}
  ]

