module EnvUnsafeLog where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving (Show, Eq)
data EnvUnsafeLog envType resType = EnvUnsafeLog (envType -> (Unsafe resType, [String]))

-- RE-ADD THE VAL STUFF?!?!

-- function that just runs the function contained in EnvUnsafeLog
runEnvUnsafeLog ::  (EnvUnsafeLog e Val String) -> e -> (Either String Val, [String])
runEnvUnsafeLog (EnvUnsafeLog eu) e = case eu e of
                                ((Error s), log) -> (Left s, log)
                                ((Ok a), log) -> (Right a, log)

-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafeLog e a String
err s = EnvUnsafeLog $ \ _ -> (Error s, [s])

-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafeLog e e String
getEnv = EnvUnsafeLog $ \ env -> (Ok env, [])

instance Functor (EnvUnsafeLog e a) where
  -- fmap :: (a -> b) -> EnvUnsafeLog a -> EnvUnsafeLog b
  fmap f (EnvUnsafeLog eu) = EnvUnsafeLog $ \ e -> 
    case eu e of
      ((Ok a), log) -> ((Ok $ f a), log)
      ((Error s), log) -> ((Error s), log)
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafeLog e a) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafeLog e a) where
  --return :: a -> EnvUnsafeLog a
  return a = EnvUnsafeLog (\ e -> ((Ok a), []))

  --(>>=) :: EnvUnsafeLog a -> (a -> EnvUnsafeLog b) -> EnvUnsafeLog b
  (EnvUnsafeLog eu) >>= f = EnvUnsafe $ \ e ->
    case eu e of
      Error s -> Error s
      Ok a -> runEnvUnsafe (f a) e

  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand