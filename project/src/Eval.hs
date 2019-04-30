module Eval where

import Data.Map (Map)
import qualified Data.Map as Map
import HelpShow
import Ast
import EnvUnsafeLog

-- the goal of the program is to return a value, what values are possible?

data Val = I Integer | B Bool | F Float | C Char 
         | Ls [Val]
         | Fun (Val -> Unsafe Val) --Fun (Val -> (Unsafe Val, [String]) ) --FIXME since this is a functional language, one thing that can be returned is a function
                                  -- FIXME This has to incorporate Writer piece, Fun (Val -> (Unsafe Val, [String]))
--int truncate
instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (F f) = show f
  show (C c) = show c
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function

instance Eq Val where
  (I x) == (I y) = (x == y)
  (B x) == (B y) = (x == y)
  (F x) == (F y) = (x == y)
  (C x) == (C y) = (x == y)
  (Ls xs) == (Ls ys) = (xs == ys)
  _ == _ = False

instance Ord Val where
  (I x) <= (I y) = (x <= y)
  (B x) <= (B y) = (x <= y)
  (F x) <= (F y) = (x <= y)
  (C x) <= (C y) = (x <= y)
  (Ls xs) <= (Ls ys) = (xs <= ys)
  _ <= _ = False

len' ::[a] -> Integer
len' []  = 0
len' (a:b) = 1 + len' b

--fromEnum2::Enum a => a -> Integer
--fromEnum2 x = x

--apply:: [Val] -> (Val->Unsafe Val) -> [Unsafe Val]

--apply:: [a] -> (a -> b) -> [b]
--apply (head:tail) f = [(f head)] ++ (apply f tail) 

toInteger2:: a -> Integer
toInteger2 = undefined


{-elem':: Val -> Val -> Bool
elem' a (Ls []) = False
elem' a (Ls (x:xs)) | (a == x)  = True
                    | otherwise = elem' a xs
-}
-- filter':: [Val] -> (Val -> Unsafe Val) -> [Val]
-- filter' [] fcn = []
-- filter' (x:xs) fcn | (fcn x) = [x] ++ (filter' xs fcn) 

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of  Ls (a:_) -> Ok a
                                    _        -> Error "can only call head on a non empty list"),                                    
   ("len",  Fun $ \ v -> case v of  Ls (ls) -> Ok $ I (len' ls)
                                    _ -> Error "not a list"),
   ("elem", undefined),--Fun $ \ v -> case v of 
                       --       Fun a -> Error "not given a value"
                         --     v' -> Ok $ Fun $ \ list -> case list of
                           --                                 Ls [ls] -> Ok $ B (elem' v' [ls])   
                                                             --helper function here list ls w/ v'
                             --                               _ -> Error "not given a list"), -- case v get v' -> ok fun \list case of lst  ls elemval v' lst
   ("map", undefined --Fun $ \v -> case v of 
             --               Fun (Fun a) -> case a of ->
              --                              Ls (b) -> Ok $ Ls (b)
                            ),
   ("filter", undefined-- Fun $ \ v -> case v of 
               --              Fun a -> Error "no"
                 --            v' -> Ok $ Fun $ \ list -> case list of
                   --                                         Ls [ls] -> Ok $ Ls (filter' v' [ls]) --helper function here list ls w/ v'
                     --                                       _ -> Error "not given a list"
                      --       _ -> Error "error"
   ), --Fun $ \ v -> case v of Ls (ls) -> Ok $ Ls ls
                          --           I a -> Ok $ I $ v a),
   ("ord", Fun $ \ v -> case v of C a -> Ok $ I (toInteger2 a)
                                  _   -> Error "not given a char"),    --char to int
   ("chr", undefined),--Fun $ \ v -> case v of I a -> Ok $ C (fromIntegral a)
             --                     _   -> Error "not given an int"),    --int to char
   ("float", Fun $ \ v -> case v of I a -> Ok $ F (fromIntegral a)
                                    _   -> Error "not given an int"),    --int to float
   ("int", Fun $ \ v -> case v of F a -> Ok $ I (truncate a)
                                  _   -> Error "not given a float")   --float to int
   ]

type Env = Map String Val

evalInt :: Ast -> EnvUnsafeLog Env Integer
evalInt a =
  do a' <- eval a
     case a' of
      I i -> return i
      _   -> err "it's not an int!!!"

evalChar:: Ast -> EnvUnsafeLog Env Char
evalChar a =
  do a' <- eval a
     case a' of
      C i -> return i
      _   -> err "it's not a char!!!"

evalFloat:: Ast -> EnvUnsafeLog Env Float
evalFloat a =
  do a' <- eval a
     case a' of
      F i -> return i
      _   -> err "it's not a float!!!"

evalBool :: Ast -> EnvUnsafeLog Env Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err "It's not bool!"

evalList:: Ast -> EnvUnsafeLog Env  [Val]
evalList a = do a' <- eval a
                case a' of
                  Ls [b] -> return [b]
                  _ -> err "It's not a list!"

evalFun :: Ast -> EnvUnsafeLog Env (Val -> Unsafe Val)
evalFun a = do a' <- eval a
               case a' of
                Fun a -> return a
                _ -> err "not a function"

getVar :: String -> EnvUnsafeLog Env Val
getVar v = do s <- getEnv
              case (Map.lookup v s) of 
                  Just i -> return i
                  Nothing -> return (I 0) 

local :: (r -> r) -> EnvUnsafeLog Env Val-> EnvUnsafeLog Env Val
local changeEnv comp  = EnvUnsafeLog (\e -> runEnvUnsafeLog comp e ) --check later because who knows

--indexInto [] _ = err "empty list"
indexInto:: Val -> Integer -> EnvUnsafeLog Env  Val
indexInto (Ls (head:tail)) 0 = case (head) of 
                                    Ls a -> return (Ls a)
                                    I a -> return (I a)
                                    B a -> return (B a)
                                    F a -> return (F a)
                                    C a -> return (C a)
                                    Fun a -> return (Fun a) -- unnecessary? probably who knows
indexInto (Ls (head:tail)) x = indexInto (Ls tail) (x - 1)
indexInto _ _ = undefined

printThis :: String -> EnvUnsafeLog Env Val -> EnvUnsafeLog Env Val
printThis x = undefined
{-
case eu e of
      (Error s, log) -> (Error s, log)
      (Ok a, log) ->

  EnvUnsafeLog $ \ e -> 
    case e of
      ((Ok a), log) -> ((Ok $ a), log ++ x)
      ((Error s), log) -> ((Error s), log ++ x)
-}
eval :: Ast -> EnvUnsafeLog Env Val
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
eval (Equal a b) = do a' <- eval a -- I'm like 95% sure these should be eval and not evalBool?!?!
                      b' <- eval b
                      return (B (a' == b'))
eval (NotEqual a b) = do a' <- eval a
                         b' <- eval b
                         return (B (a' /= b'))  
eval (LessThan l r) = 
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ B $ f1 < f2
                  I i2 -> err "can not compare float with an integer"
                  _    -> err "can only compare floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not compare an integer with a float" 
                  I i2 -> return $ B $ i1 < i2
                  _    -> err "can only compare floats and ints"
      _    -> err "can only compare floats and ints"    
eval (LessThanOrEqual l r) = 
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ B $ f1 <= f2
                  I i2 -> err "can not compare float with an integer"
                  _    -> err "can only compare floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not compare an integer with a float" 
                  I i2 -> return $ B $ i1 <= i2
                  _    -> err "can only compare floats and ints"
      _    -> err "can only compare floats and ints"                         
eval (GreaterThan l r) = 
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ B $ f1 > f2
                  I i2 -> err "can not compare float with an integer"
                  _    -> err "can only compare floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not compare an integer with a float" 
                  I i2 -> return $ B $ i1 > i2
                  _    -> err "can only compare floats and ints"
      _    -> err "can only compare floats and ints"                                                
eval (GreatThanOrEqual l r) = 
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ B $ f1 >= f2
                  I i2 -> err "can not compare float with an integer"
                  _    -> err "can only compare floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not compare an integer with a float" 
                  I i2 -> return $ B $ i1 >= i2
                  _    -> err "can only compare floats and ints"
      _    -> err "can only compare floats and ints"  
                            --   return (B (a' >= b'))  
eval (ValChar i) = return $ C i
eval (ValInt i) = return $ I i
eval (Nil) = return $ Ls []
eval (Mult l r) = --change to work for floats and ints
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ F $ f1 * f2
                  I i2 -> err "can not multiply a float with an integer" --return $ F $ f1 + i2
                  _    -> err "can only multiply floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not multiply an integer with a float" --return $ F $ i1 + f2
                  I i2 -> return $ I $ i1 * i2
                  _    -> err "can only multiply floats and ints"
      _    -> err "can only multiply floats and ints"
eval (Plus l r) =       --change to work for floats and ints
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ F $ f1 + f2
                  I i2 -> err "can not add a float with an integer"
                  _    -> err "can only add floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not add an integer with a float" --return $ F $ i1 + f2
                  I i2 -> return $ I $ i1 + i2
                  _    -> err "can only add floats and ints"
      _    -> err "can only add floats and ints"
eval (Minus l r) =      --change to work for floats and ints
  do a <- eval l
     b <- eval r
     case (a) of
      F f1 -> case (b) of
                  F f2 -> return $ F $ f1 - f2
                  I i2 -> err "can not add a float with an integer"
                  _    -> err "can only add floats and ints"
      I i1 -> case (b) of
                  F f2 -> err "can not add an integer with a float" --return $ F $ i1 + f2
                  I i2 -> return $ I $ i1 - i2
                  _    -> err "can only add floats and ints"
      _    -> err "can only add floats and ints"
eval (Div l r) = do l' <- evalInt l         --should be for ints
                    r' <- evalInt r
                    case r' of
                      0 -> err "Dividing by zero error"
                      x -> return $ I $ l' `div` r'   --FIXME!
eval (DivFloat l r) = do l' <- evalFloat l         --should be for floats
                         r' <- evalFloat r
                         case r' of
                           0 -> err "Dividing by zero error"
                           x -> return $ F $ l' / r'
eval (ValBool a) = return $ B a
eval (Not a) = do a' <- (evalBool a)
                  case (a') of
                          True -> return $ B False
                          False -> return $ B True
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
eval (Letrec v val bod) = undefined --TODO
eval (DotMixIn a b) =  undefined--(\x -> eval (Lam ((evalFun a) (Lam (evalFun b) x)))) --FIXME
-- eval (Lam x bod) = do env <- getEnv
--                       return $ Fun $ \ v -> runEnvUnsafeLog (eval bod) (Map.insert x v env)
eval (Lam x bod) = undefined
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
run a = runEnvUnsafeLog (eval a) -}

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = case runEnvUnsafeLog (eval a) stdLib of
        (Error s,log) -> (Left s, log)
        (Ok a, log)   -> (Right a, log)


