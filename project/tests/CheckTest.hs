module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Check

-- provide tests that show your check works

checktest = testGroup "CheckTest" 
  [
  error "no tests yet!"
  -- ...
  ]

