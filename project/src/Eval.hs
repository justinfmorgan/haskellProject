module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import EnvUnsafe

-- the goal of the program is to return a value, what values are possible?
--data Val -- ...

data Val = I Integer | B Bool | F Float | C Char | S String
         | Ls [Val]
         | Fun (Val -> Unsafe Val) --FIXME since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show = undefined


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = undefined 

