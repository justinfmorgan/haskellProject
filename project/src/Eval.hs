module Eval where

import Data.Map (Map)
import qualified Data.Map as Map
--import Prelude (fromEnum, toEnum)
import HelpShow
import Ast
import EnvUnsafe

-- the goal of the program is to return a value, what values are possible?

data Val = I Integer | B Bool | F Float | C Char 
         | Ls [Val]
         | Fun (Val -> Unsafe Val) --FIXME since this is a functional language, one thing that can be returned is a function
                                  -- FIXME This has to incorporate Writer piece, Fun (Val -> (Unsafe Val, [String]))

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (F f) = show f
  show (C c) = show c
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function
  
len' ::[a] -> Integer
len' []  = 0
len' (a:b) = 1 + len' b

--fromEnum2::Enum a => a -> Integer
--fromEnum2 x = x

--apply:: [Val] -> (Val->Unsafe Val) -> [Unsafe Val]

--apply:: [a] -> (a -> b) -> [b]
--apply (head:tail) f = [(f head)] ++ (apply f tail) 

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of  Ls (a:_) -> Ok a
                                    _        -> Error "can only call head on a non empty list"), 
                                  
                                   
   ("len",  Fun $ \ v -> case v of  Ls (ls) -> Ok $ I (len' ls)
                                    _ -> Error "not a list"),
   ("elem", undefined),
   ("map", undefined --Fun $ \v -> case v of 
             --               Fun (Fun a) -> case a of ->
              --                              Ls (b) -> Ok $ Ls (b)
                            ),
   ("filter", undefined), --Fun $ \ v -> case v of Ls (ls) -> Ok $ Ls ls
                          --           I a -> Ok $ I $ v a),
   ("ord", undefined),-- Fun $ \ v -> case v of C a -> Ok $ I $ fromEnum2 a
            --                      _   -> Error "not a char"), ---char to int
   ("chr", undefined),  --int to char
   ("float", Fun $ \ v -> case v of I a -> Ok $ F (fromIntegral a)
                                    _   -> Error "not given an int"),    --int to float
   ("int", Fun $ \ v -> case v of F a -> Ok $ I (round a)
                                  _   -> Error "not given a float")   --float to int
   ]--Fun $ \ v -> case v of Ls [] -> Error "can only call len on a non empty list"
             --                      Ls (ls:l) -> Ok $ Ls [ls] )]

type Env = Map String Val

evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a =
  do a' <- eval a
     case a' of
      I i -> return i
      _   -> err "it's not an int!!!"

evalChar:: Ast -> EnvUnsafe Env Char
evalChar a =
  do a' <- eval a
     case a' of
      C i -> return i
      _   -> err "it's not a char!!!"

evalFloat:: Ast -> EnvUnsafe Env Float
evalFloat a =
  do a' <- eval a
     case a' of
      F i -> return i
      _   -> err "it's not a float!!!"

--evalIntOrFloat :: Ast -> EnvUnsafe Env b
--evalIntOrFloat a = 
 -- do a' <- eval a
   --  case a' of
    --  F f -> return f
    --  I i -> return i
     -- _   -> err "it's not a float or int!!!"    
evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err "It's not bool!"

evalList:: Ast -> EnvUnsafe Env  [Val]
evalList a = do a' <- eval a
                case a' of
                  Ls [b] -> return [b]
                  _ -> err "It's not a list!"

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = do a' <- eval a
               case a' of
                Fun a -> return a
                _ -> err "not a function"

getVar :: String -> EnvUnsafe Env Val
getVar v = do s <- getEnv
              case (Map.lookup v s) of 
                  Just i -> return i
                  Nothing -> return (I 0) 

local :: (r -> r) -> EnvUnsafe Env Val-> EnvUnsafe Env Val
local changeEnv comp  = EnvUnsafe (\e -> runEnvUnsafe comp e ) --check later because who knows

--indexInto [] _ = err "empty list"
indexInto:: Val -> Integer -> EnvUnsafe Env  Val
indexInto (Ls (head:tail)) 1 = case (head) of 
                                    Ls a -> return (Ls a)
                                    I a -> return (I a)
                                    B a -> return (B a)
                                    F a -> return (F a)
                                    C a -> return (C a)
                                    Fun a -> return (Fun a) -- unnecessary? probably who knows
indexInto (Ls (head:tail)) x = indexInto (Ls tail) (x - 1)
indexInto _ _ = undefined

--printThis :: x -> PrinterMonad x ()
--printThis x = PrinterMonad [x] ()

eval :: Ast -> EnvUnsafe Env Val
eval (ValFloat i) = return $ F i
eval (Separator l r) = 
    do x <- eval l
       y <- eval r
       return (y)   
eval (Concat a b) =
    do a' <- evalList a
       b' <- evalList b
       return $ Ls $ a' ++ b'
eval (IntExp a b) =
  do l' <- evalInt a
     r' <- evalInt b
     return $ I $ l' ^ r'    
eval (FloatExp a b) =
  do l' <- evalFloat a
     r' <- evalFloat b
     return $ F $ l' ** r'
eval (Print x) = undefined --do
--    x' <- eval x
--    printThis x'
--    return (x')
eval (Modulus a b) =   --for ints
  do l' <- evalInt a
     r' <- evalInt b
     return $ I $ l' `mod` r' 
eval (ListIndex a b) =
    do a' <- evalList a
       b' <- evalInt b 
       let length = len' a'
       if length < b' then err "List is not big enough" else (indexInto (Ls a') b') 
eval (Equal a b) = do a' <- evalBool a
                      b' <- evalBool b
                      return (B (a' == b'))
eval (NotEqual a b) = do a' <- evalBool a
                         b' <- evalBool b
                         return (B (a' /= b'))  
eval (LessThan a b) = do a' <- evalBool a               --FIXME
                         b' <- evalBool b
                         return (B (a' < b'))
eval (LessThanOrEqual a b) = do a' <- evalBool a               --FIXME
                                b' <- evalBool b
                                return (B (a' <= b'))                         
eval (GreaterThan a b) = do a' <- evalBool a               --FIXME
                            b' <- evalBool b
                            return (B (a' > b'))                                                
eval (GreatThanOrEqual a b) = do a' <- evalBool a               --FIXME
                                 b' <- evalBool b
                                 return (B (a' >= b'))  
eval (ValChar i) = return $ C i
eval (ValInt i) = return $ I i
eval (Nil) = return $ Ls []
eval (Mult l r) = --change to work for floats and ints
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' * r'
eval (Plus l r) =       --change to work for floats and ints
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' + r'
eval (Minus l r) =      --change to work for floats and ints
  do l' <- evalInt l
     r' <- evalInt r
     return $ I $ l' - r'
eval (Div l r) = do l' <- evalInt l         --should be for ints
                    r' <- evalInt r
                    case r' of
                      0 -> err "Dividing by zero error"
                      x -> return $ I $ l' `div` r'
eval (DivFloat l r) = do l' <- evalFloat l         --should be for floats
                         r' <- evalFloat r
                         case r' of
                           0 -> err "Dividing by zero error"
                           x -> return $ F $ l' / r'
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
{-
-- THIS NEEDS TO BE FIXED!!!!
-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = runEnvUnsafe (eval a) -}

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> Unsafe a  -- ^ (error message or result value, all the printings)
run a = runEnvUnsafe (eval a) 


