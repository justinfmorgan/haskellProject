module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import Check
import ParserMonad


data LangOut = 
    ParseError -- ^ retuned when the string could not be parsed
  | RuntimeError String [String]
  -- ^ returned when there is a runtime error
  -- first String is the error message
  -- this list of Strings is what is printed before the error was encountered 
  | Ok Val [String]
  | TypeError String
  | UndefinedVarError String    --will stop execute as this is an error, more defined error message is shown from calling warn
  
  -- ^ retuned when the program runs successfully and return a value
  -- The Val is the evaluation result of the program
  -- The list of String is what gets printed while running the program
  deriving Show

instance Eq LangOut where
   (Ok v1 xs) == (Ok v2 ys) = (v1 == v2) && (xs == ys)
   ParseError == ParseError = True
   RuntimeError x xs == RuntimeError y ys = (x == y) && (xs == ys)
   _ == _ = False -- FIX THIS!!!
          

--getType :: Ast -> LangOut


---run output => (Either String Val, [String]) 
-- | execute the program as a string and get the result
exec :: Ast -> LangOut
exec ast = case run ast of
           (Left a, b) -> RuntimeError a b
           (Right val, b) -> Ok val b

-- Does not work with stdLib Functions
execute :: String -> LangOut --will stop if there is an undefined variable being use because this is an error!
execute s = case (parse parser) s of
  Just (ast,"") -> if (errorMsg ast == Set.empty) then case run ast of 
                                                          (Left a, b) -> RuntimeError a b
                                                          (Right val, b) -> Ok val b
                                                  else UndefinedVarError "undefined variable in usage"
  _  -> ParseError

-- Works with stdLib functions but does not produce UndefinedVarError
execute' :: String -> LangOut
execute' s = case (parse parser) s of
  Just (ast,"") ->case run ast of 
                       (Left a, b) -> RuntimeError a b
                       (Right val, b) -> Ok val b
  _  -> ParseError


-- | perform static checking on the program string, may be empty if there is a parse error
warn :: String -> (Set WarningMsg) 
warn s = case (parse parser) s of
  Just (ast,"") -> check ast
  _             -> Set.empty
