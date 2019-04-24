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

len' []  = 0
len' (a: b) = 1 + len' b

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of  Ls (a:_) -> Ok a
                                    _        -> Error "can only call head on a non empty list"), 
                                  --Ls ((B a): _)   -> Ok $ (B a)
                                  -- Ls ((I a): _)   -> Ok $ (I a)
                                   --Ls ((Fun a): _) -> Ok $ (Fun a) 
                                   --Ls ((Ls (a:b)): _) -> Ok $ (a)
                                   
   ("len",  Fun $ \ v -> case v of  Ls (ls) -> Ok $ I (len' ls)
                                    _ -> Error "not a list"
   )]--Fun $ \ v -> case v of Ls [] -> Error "can only call len on a non empty list"
             --                      Ls (ls:l) -> Ok $ Ls [ls] )]


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = undefined 

