module EnvUnsafeLog where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving Show

instance Eq a => Eq (Unsafe a) where
  (Error x) == (Error y) = (x == y)
  (Ok x) == (Ok y) = (x == y)
  _ == _ = False

data EnvUnsafeLog envType resType = EnvUnsafeLog (envType -> (Unsafe resType, [String]))

-- function that just runs the function contained in EnvUnsafe
runEnvUnsafeLog ::  (EnvUnsafeLog e a) -> e -> (Unsafe a, [String])
runEnvUnsafeLog (EnvUnsafeLog eu) e = eu e

-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafeLog e a
err s = EnvUnsafeLog $ \ _ -> (Error s, [])


-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafeLog e e
getEnv = EnvUnsafeLog $ \ env -> (Ok env, [])


instance Functor (EnvUnsafeLog e) where
  -- fmap :: (a -> b) -> EnvUnsafeLog a -> EnvUnsafeLog b
  fmap f (EnvUnsafeLog eu) = EnvUnsafeLog $ \ e -> 
    case eu e of
      ((Ok a), log) -> ((Ok $ f a), log)
      ((Error s), log) -> ((Error s), log)
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafeLog e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafeLog e) where
  --return :: a -> EnvUnsafeLog a
  return a = EnvUnsafeLog (\ e -> ((Ok a), []))

  --(>>=) :: EnvUnsafeLog a -> (a -> EnvUnsafeLog b) -> EnvUnsafeLog b
  (EnvUnsafeLog eu) >>= f = EnvUnsafeLog $ \ e ->
    case eu e of
      (Error s, log) -> (Error s, log)
      (Ok a, log) -> let (EnvUnsafeLog g) = f a
                     in case g e of
                        (Error s', log2) -> (Error s', log ++ log2)
                        (Ok a', log2)    -> (Ok a', log ++ log2)


  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand