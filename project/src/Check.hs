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

data WarningMsg = UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name => unbound 
                | UnusedVar String -- => bound but not 
                | TypeError String --types don't match => Noah
  -- ...
  deriving (Show,Eq)

  {- 
  warning notes
  undefinedvaruse => using isClosed can apply to lambdas, var string, app looking for unb
  unusedvar => defined var in a lambda that isn't used => new helper fcn? applies to lambdas, var string, app
  typeError => when the 2 asts for the function are incorrect can apply to
  -and, or, not, mult, add, sub, greaterthan, lessthan, equal, notequal, greaterthanequal, lessthanequal, basically everything but lambda, app

  -}

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast/
check :: Ast -> Set WarningMsg
check (Var a) = Set.singleton (UndefinedVarUse "unused variable") --FIXME 
--check (Lam str b ) | (isClosed (Lam str b) == False) = UndefinedVarUse "unused variable" --FIXME should say what variable is not used
--				   | 
--check (App a b) | (isClosed (App a b) == False) = UndefinedVarUse "unused variable" --FIXME should say what variable is not used
--				| 
-- collect all the vars that appear in the expression that are not bound
freeVars :: Ast -> Set String
freeVars (Var s) = Set.singleton s
freeVars (App t1 t2) = Set.union (freeVars t1) (freeVars t2)
freeVars (Lam s t1) = Set.delete s (freeVars t1)

-- when are there no free variables in a lambda expression?
isClosed :: Ast -> Bool
isClosed t = ((freeVars t) == Set.empty)