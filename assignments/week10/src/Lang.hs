module Lang where

import Data.Map (Map)
import qualified Data.Map as Map

import HelpShow

import EnvUnsafe


-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
           deriving (Eq,Show) -- helpful to use this during testing
--         deriving Eq

--instance Show Ast where
 -- show ast = showPretty ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool
         | Ls [Val]
         | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function

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

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> Unsafe Val
run a = runEnvUnsafe (eval a) stdLib

type Env = Map String Val

-- some helper function, you may find helpful
valOf :: String -> EnvUnsafe Env Val
valOf var = undefined

-- add a val into the environment
withVal :: String -> Val -> EnvUnsafe Env a -> EnvUnsafe Env a
withVal var v comp = undefined

-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a =
  do a' <- eval a
     case a' of
      I i -> return i
      _   -> err "it's not int!!!"

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err "It's not bool!"
--evalBool a = return $ B a

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = do a' <- eval a
               case a' of
                Fun a -> return a
                _ -> err "not a function"

getVar :: String -> EnvUnsafe Env Val
getVar v = do s <- getEnv
              case (Map.lookup v s) of 
                  Just i -> return i
                  Nothing -> return (I 0)  --potentially an issue?

local :: (r -> r) -> EnvUnsafe Env Val-> EnvUnsafe Env Val
local changeEnv comp  = EnvUnsafe (\e -> runEnvUnsafe comp e ) --check later because who knows

eval :: Ast -> EnvUnsafe Env Val
eval (ValInt i) = return $ I i
eval (Nil) = return $ Ls []
eval (Mult l r) =
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' * r'
eval (Plus l r) =
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' + r'
eval (Minus l r) =
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' - r'
eval (Div l r) = do l' <- evalInt l
                    r' <- evalInt r
                    case r' of
                      0 -> err "Dividing by zero error"
                      x -> return $ I $ l' `div` r'
eval (ValBool a) = return $ B a
eval (Not a) = do a' <- (evalBool a)
                  return $ B $ a'
eval (And a b) = do a' <- evalBool a
                    b' <- evalBool b
                    return (B (a' && b'))
eval (Or a b) = do a' <- evalBool a
                   b' <- evalBool b
                   return ( B ( a' || b') )
eval (Cons a b) = do l <- eval a 
                     r <- eval b
                     case (r) of
                      Ls a -> return $ Ls $[l] ++ a
                      _    -> err "type mismatch"
eval (Var str) = getVar str
eval (If a b c) = do a' <- (evalBool a) 
                     case (a') of
                          True -> (eval b)
                          False -> (eval c)
eval (Let v val bod) = 
  do val' <- eval val
     local (Map.insert v val') (eval bod)
eval (Lam x bod) = do env <- getEnv
                      return $ Fun $ \ v -> runEnvUnsafe (eval bod) (Map.insert x v env)
eval (App e1 e2) = do e1' <- (evalFun e1)
                      e2' <- eval e2 --apply e1' onto e2', check to see if its broken or not -> return a val
                      case (e1' e2') of
                         Ok a -> return a
                         _ -> err "error did not apply"

ex1 = run $ (ValInt 4) `Mult` (ValInt 2)
ex2 = run $ (ValInt 4) `Mult` Nil


-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)

showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)


