module Check where

import Data.Set (Set, elems, union, difference)
import qualified Data.Set as Set

import Ast
import EnvUnsafe

-- here you can preform static checks

-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors

data WarningMsg = UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name => unbound *** should be an error
                | UnusedVar String -- => bound but not used Mixin Alex
                | TypeError String --types don't match => Mixin Noah
  -- ...
  deriving (Show,Eq, Ord)

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
check (Var a) = Set.map f (freeVars (Var a)) 
check (ValBool a) = Set.empty
check (ValInt a) = Set.empty
check (ValFloat a) = Set.empty
check (Nil) = Set.empty
check (ValChar a) = Set.empty
{-check (And a b) = undefined
check (Or a b) = undefined
check (Not a) = undefined-}
{-check (Plus a b) = undefined
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
check (If a b c) = undefined
check (Let str b c) = undefined
check (DotMixIn a b) = undefined
check (Letrec str b c) = undefined -}
check (Lam a b) = Set.union ( Set.map f (freeVars (Lam a b)) ) (Set.map  f2 (difference (boundVars (Lam a b)) (used (Lam a b)) ))
check (App a b) = Set.union ( Set.map f (freeVars (App a b)) ) (Set.map  f2 (difference (boundVars (App a b)) (used (App a b)) ))
check x = Set.union ( Set.map f (freeVars (x)) ) (Set.map  f2 (difference (boundVars (x) )(used (x)) ))-- (Set.map f2 (difference (boundVars x) (used x )))

{- unused
finalUnused:: Ast -> Set WarningMsg
finalUnused x = Set.map  f2 (difference (boundVars x) (used x) )

unusedBoundVars:: Set String -> Set String -> Set String
unusedBoundVars x y = difference x y
-}

boundVars :: Ast -> Set String --gets all the boundvars now must figure out which of these aren't used
boundVars (Lam v bod) = Set.insert v $ boundVars bod
boundVars (Var _) = Set.empty
boundVars (App f a) = boundVars f `Set.union` boundVars a
boundVars _ = Set.empty

used:: Ast -> Set String
used (Var a) = Set.singleton a
used (Lam a b) = used b
used (App a b) = Set.union (used a) (used b)
used (ValBool a) = Set.empty
used (ValInt a) = Set.empty
used (ValFloat a) = Set.empty
used (And a b) =  Set.union (used a) (used b)
used (Or a b) =  Set.union (used a) (used b)
used (Not a) = (used a)
used (ValChar a) = Set.empty --FIXME i think this should be added - figure out how later
used (Plus a b) =  Set.union (used a) (used b)
used (Minus a b) =  Set.union (used a) (used b)
used (Mult a b) =  Set.union (used a) (used b)
used (Div a b) =  Set.union (used a) (used b)
used (DivFloat a b) =  Set.union (used a) (used b)
used (Modulus a b) =  Set.union (used a) (used b)
used (Equal a b) =  Set.union (used a) (used b)
used (NotEqual a b) =  Set.union (used a) (used b)
used (GreatThanOrEqual a b) =  Set.union (used a) (used b)
used (GreaterThan a b) =  Set.union (used a) (used b)
used (LessThanOrEqual a b) =  Set.union (used a) (used b)
used (LessThan a b) =  Set.union (used a) (used b)
used (Cons a b) =  Set.union (used a) (used b)
used (Separator a b) =  Set.union (used a) (used b)
used (Concat a b) =  Set.union (used a) (used b)
used (IntExp a b) =  Set.union (used a) (used b)
used (FloatExp a b) =  Set.union (used a) (used b)
used (ListIndex a b) = (used a)
used (Print a) = (used a)
used (Nil) = Set.empty
used (If a b c) = Set.union (Set.union (used a)(used b)) (used c)
used (Let str b c) = Set.union (used b) (used c)
used (DotMixIn a b) = Set.union (used a) (used b)
used (Letrec str b c) = Set.union (used b) (used c)

f :: String -> WarningMsg -- 0 means undefinedvaruse, 1 means unusedvar
f x = UndefinedVarUse ("unbound variable " ++ x)

f2:: String -> WarningMsg 
f2 x = UnusedVar ("bound but unused variable " ++ x )

freeVars :: Ast -> Set String
freeVars (Var s) = Set.singleton s
freeVars (App t1 t2) = Set.union (freeVars t1) (freeVars t2)
freeVars (Lam s t1) = Set.delete s (freeVars t1)
freeVars _ = Set.empty --FIXME do we need all the cases of ast??

-- when are there no free variables in a lambda expression?
isClosed :: Ast -> Bool
isClosed t = ((freeVars t) == Set.empty)