module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import EnvUnsafe

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors

data WarningMsg = UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
                | UnusedVar String
                | TypeError String

  -- ...
  deriving (Show,Eq)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check = undefined

-- collect all the vars that appear in the expression that are not bound
freeVars :: Ast -> Set String
freeVars (Var s) = Set.singleton s
freeVars (App t1 t2) = Set.union (freeVars t1) (freeVars t2)
freeVars (Lam s t1) = Set.delete s (freeVars t1)

-- when are there no free variables in a lambda expression?
isClosed :: Ast -> Bool
isClosed t = ((freeVars t) == Set.empty)