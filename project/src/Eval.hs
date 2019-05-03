module Eval where

import Data.Map (Map)
import qualified Data.Map as Map
import HelpShow
import Ast
import EnvUnsafeLog
import Data.Char (ord, chr)
-- the goal of the program is to return a value, what values are possible?

data Val = I Integer | B Bool | F Double | C Char 
         | Ls [Val]
         | Fun (Val -> (Unsafe Val, [String]))

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

-- rename a free var, in a term
rename :: Ast -> String -> String -> Ast
rename (App f a)      from to             = App (rename f from to) (rename a from to)
rename (Lam v bod)    from to | v == from = Lam v bod
rename (Lam v bod)    from to | otherwise = Lam v $ rename bod from to
rename (Var v)        from to | v == from = Var to
                              | otherwise = Var v

len' ::[a] -> Integer
len' []  = 0
len' (a:b) = 1 + (len' b)

elem':: Val -> Val -> Bool
elem' a (Ls []) = False
elem' a (Ls (x:xs)) | (a == x)  = True
                    | otherwise = elem' a (Ls xs)
elem' _ _ = undefined

(+++):: (Unsafe Val, [String]) -> (Unsafe Val, [String]) -> (Unsafe Val, [String])
(+++) (Ok (Ls []), log) (Ok (Ls []), log2) = (Ok (Ls []), log++log2)
(+++) (Ok (Ls x), log) (Ok (Ls y), log2) = (Ok (Ls (x ++ y)), (log ++ log2))
(+++) _ _ = (Ok (Ls []), [])

filter':: (Val ->(Unsafe Val, [String])) -> Val -> (Unsafe Val, [String])
filter' _ (Ls []) = (Ok(Ls []),[])
filter' f (Ls (x:xs)) = case (f x) of
                          (Ok (B True),log)  -> (Ok (Ls [x]), log) +++ (filter' f (Ls xs))
                          (Ok (B False),log) -> filter' f (Ls xs)
                          _                  -> (Error "filter has not been given a function and list", [])
filter' _ _ = undefined

map':: (Val ->(Unsafe Val, [String])) -> Val -> (Unsafe Val, [String])
map' _ (Ls []) = (Ok (Ls []), [])
map' f (Ls (x:xs)) = (f (Ls [x])) +++ (map' f (Ls xs))
map' _ _ = undefined


stdLib = Map.fromList
  [
   ("tail", Fun $ \ v -> case v of Ls (_:ls) -> (Ok $ Ls ls, [])
                                   _         -> (Error "can only call tail on a non empty list",[])),
   ("head", Fun $ \ v -> case v of  Ls (a:_) -> (Ok a, [])
                                    _        -> (Error "can only call head on a non empty list",[])),                                    
   ("len",  Fun $ \ v -> case v of  Ls (ls) -> (Ok $ I (len' ls), [])
                                    _ -> (Error "not a list", [])),                           
   ("elem", Fun $ \v -> (Ok (Fun $ \v2 -> (Ok $ B (elem' v v2), [])), [])),--Fun $ \ v -> case v of 
   ("map", Fun $ \v ->case v of 
                              Fun a -> (Ok $ Fun $ \list -> case list of
                                                                  Ls ls ->(map' a (Ls ls)), [])),
   ("filter", Fun $ \v ->case v of 
                              Fun a -> (Ok $ Fun $ \list -> case list of
                                                                  Ls ls ->(filter' a (Ls ls)), [])),
   ("ord", Fun $ \ v -> case v of C a -> (Ok $ I (fromIntegral $ ord a), [])
                                  _   -> (Error "not given a char", [])),    --char to int
   ("chr", Fun $ \ v -> case v of I a -> (Ok $ C ( chr (fromIntegral a)),[])
                                  _   -> (Error "not given an int",[])),    --int to char
   ("float", Fun $ \ v -> case v of I a -> (Ok $ F (fromIntegral a), [])
                                    _   -> (Error "not given an int", [])),    --int to float
   ("int", Fun $ \ v -> case v of F a -> (Ok $ I (truncate a), [])
                                  _   -> (Error "not given a float", []))   --float to int
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

evalFloat:: Ast -> EnvUnsafeLog Env Double
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

evalFun :: Ast -> EnvUnsafeLog Env (Val -> (Unsafe Val, [String]))
evalFun a = do a' <- eval a
               case a' of
                Fun a -> return a
                _ -> err "not a function"

getVar :: String -> EnvUnsafeLog Env Val
getVar v = do s <- getEnv
              case (Map.lookup v s) of 
                  Just i -> return i
                  Nothing -> return (I 0)

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

eval :: Ast -> EnvUnsafeLog Env Val
eval (ValFloat i) = return $ F i
eval (Separator l r) = 
    do x <- eval l
       y <- eval r
       return (y)   
eval (Concat a b) =
    do a' <- eval a
       b' <- eval b
       case (a') of
             Ls ls1 -> case (b') of
                        Ls ls2 -> return $ Ls $ ls1 ++ ls2
                        _      -> err "not given a list!" 
             _      -> err "not given a list!" 
eval (IntExp a b) =
  do l' <- evalInt a
     r' <- evalInt b
     return $ I $ l' ^ r'    
eval (FloatExp a b) =
  do l' <- evalFloat a
     r' <- evalFloat b
     return $ F $ l' ** r'
eval (Print x) = do x' <- eval x
                    EnvUnsafeLog (\e -> (Ok x', [show x']))
eval (Modulus a b) =   --for ints
  do l' <- evalInt a
     r' <- evalInt b
     return $ I $ l' `mod` r' 
eval (ListIndex a b) =
    do a' <- eval a
       b' <- evalInt b 
       case (a') of Ls a -> if (len' a) < b' then err "List is not big enough" else (indexInto (a') b') 
                    _       -> err "did not give a list!"

eval (Equal a b) = do a' <- eval a -- I'm like 95% sure these should be eval and not evalBool?!?!
                      b' <- eval b
                      return (B (a' == b'))
eval (NotEqual a b) = do a' <- eval a
                         b' <- eval b
                         return (B (a' /= b'))  
eval (LessThan a b) = do a' <- eval a               --FIXME
                         b' <- eval b
                         return (B (a' < b'))
eval (LessThanOrEqual a b) = do a' <- eval a               --FIXME
                                b' <- eval b
                                return (B (a' <= b'))                         
eval (GreaterThan a b) = do a' <- eval a               --FIXME
                            b' <- eval b
                            return (B (a' > b'))                                                
eval (GreatThanOrEqual a b) = do a' <- eval a               --FIXME
                                 b' <- eval b
                                 return (B (a' >= b'))  
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
eval (If a b c) = do a' <- (eval a) 
                     case (a') of
                          B True -> (eval b)
                          B False -> (eval c)
                          I x -> if (x > 0) then (eval b) else (eval c)
                          F x -> if (x > 0) then (eval b) else (eval c)
                          _   -> err "if requires a bool, int or float!"
eval (Let v val bod) = eval (App (Lam v bod) val)
--eval (Letrec v val bod) = let f = Fun $ \e -> withVal v f (do f' <- eval val; f' e)
                          --in withVal v f (eval bod)
eval (DotMixIn f g) = eval (Lam "x" (App f (App g (Var "x"))))
eval (Lam x bod) = do env <- getEnv
                      return $ Fun $ \ v -> runEnvUnsafeLog (eval bod) (Map.insert x v env)
eval (App e1 e2) = do e1' <- (evalFun e1)
                      e2' <- eval e2 --apply e1' onto e2', check to see if its broken or not -> return a val
                      case (e1' e2') of
                         (Ok a, _) -> return a
                         _ -> err "error did not apply"
                         
-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = case runEnvUnsafeLog (eval a) stdLib of
        (Error s,log) -> (Left s, log)
        (Ok a, log)   -> (Right a, log)


