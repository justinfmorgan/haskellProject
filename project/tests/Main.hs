module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import System.Environment

import CheckTest
import EvalTest
import ParserTest
main = 
    do 
        setEnv "TASTY_TIMEOUT" "40s"
        setEnv "TASTY_QUICKCHECK_TESTS" "1000" --TODO: I never trust less than 10000
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain testSuite
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"


testSuite =
  testGroup
    "allTests"
    [
    --CheckTest.tests,
    --EvalTest.tests,
    ParserTest.tests
    -- ...
    ]
