module Check where

import Data.Set (Set, elems, union)
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
  MIXIN unusedvar => defined var in a lambda that isn't used => new helper fcn? applies to lambdas, var string, app
  MIXIN typeError => when the 2 asts for the function are incorrect can apply to
  -and, or, not, mult, add, sub, greaterthan, lessthan, equal, notequal, greaterthanequal, lessthanequal, basically everything but lambda, app

  -}


-- | perform static checking on the Ast
-- the output a set of warning on that input Ast/
check :: Ast -> Set WarningMsg
check (Var a) = Set.singleton (UndefinedVarUse ("unused variable " ++ a)) --FIXME 
check (ValBool a) = Set.empty
check (ValInt a) = Set.empty
check (ValFloat a) = Set.empty
check (And a b) = undefined
check (Or a b) = undefined
check (Not a) = undefined
check (ValChar a) = Set.empty
check (Plus a b) = undefined
check (Minus a b) = undefined
check (Mult a b) = undefined
check (Div a b) = undefined
check (DivFloat a b) = undefined 
check (Modulus a b) = undefined
check (Equal a b) = undefined
check (NotEqual a b) = undefined
check (GreatThanOrEqual a b) = undefined
check (GreaterThan a b) = undefined
check (LessThanOrEqual a b) = undefined
check (LessThan a b) = undefined
check (Cons a b) = undefined
check (Separator a b) = undefined
check (Concat a b) = undefined
check (IntExp a b) = undefined
check (FloatExp a b) = undefined
check (ListIndex a b) = undefined
check (Print a) = undefined
check (Nil) = Set.empty
check (If a b c) = undefined
check (Let str b c) = undefined
check (DotMixIn a b) = undefined
check (Letrec str b c) = undefined
check (Lam a b) = undefined --test f (freeVars (Lam a b))--undefined
check (App a b) = undefined
--check (Lam str b ) | (isClosed (Lam str b) == False) = UndefinedVarUse "unused variable" --FIXME should say what variable is not used
--				   | 
--check (App a b) | (isClosed (App a b) == False) = UndefinedVarUse "unused variable" --FIXME should say what variable is not used
--				| 
-- collect all the vars that appear in the expression that are not bound
test f x = Set.map f x

f:: String -> WarningMsg
f x = UndefinedVarUse ("Unused variable " ++ x)
--toWarning:: Set String -> Set WarningMsg
--toWarning 
--freeVarsToWarning:: [String] -> Set String
--freeVarsToWarning [] = Set.empty
--freeVarsToWarning (x:xs) = Set.union (freeVarsToWarning [x]) (freeVarsToWarning xs)
--freeVarsToWarning [x] = Set.singleton (x)--UndefinedVarUse ("unused variable ")) --Fixme state the variable name

freeVars :: Ast -> Set String
freeVars (Var s) = Set.singleton s
freeVars (App t1 t2) = Set.union (freeVars t1) (freeVars t2)
freeVars (Lam s t1) = Set.delete s (freeVars t1)
freeVars _ = undefined

-- when are there no free variables in a lambda expression?
isClosed :: Ast -> Bool
isClosed t = ((freeVars t) == Set.empty)