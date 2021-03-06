module Ast where

import HelpShow


-- | the abstract syntax tree for the language
data Ast = ValBool Bool -- Added
         | And Ast Ast | Or Ast Ast | Not Ast
         
         | ValFloat Double -- added
         | ValChar Char -- added
--         | List [Ast] -- added

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast --Done except div, must add float for plus, minus, mult
            --Alex div 
         | Separator Ast Ast  --- all added Noah Done i think

         | Equal Ast Ast | NotEqual Ast Ast --Alex Done
         | LessThan Ast Ast | LessThanOrEqual Ast Ast --Alex  Done 
         | GreaterThan Ast Ast | GreatThanOrEqual Ast Ast    --Alex Done

         | Concat Ast Ast  --Alex Done
         | DivFloat Ast Ast  --Alex Done
         | Modulus Ast Ast -- only for integers Noah Done
         | FloatExp Ast Ast  --Noah Done
         | IntExp Ast Ast  --Noah Done
         | ListIndex Ast Ast -- left -> list, right -> integer --Alex   Done??
         | Print Ast --needs to be figured out first :(

         | Nil
         | Cons Ast Ast --Noah  -- done

         | If Ast Ast Ast   --done
         | Let String Ast Ast --done

         | Var String  --Justin 
         | Lam String Ast  --Justin
         | App Ast Ast  --Justin
            --mixins
--         | Letrec String Ast Ast
         | DotMixIn Ast Ast -- f . g instead of \x -> f (g x) TODO add to evaltest, eval

           deriving (Show,Eq) -- helpful to use this during testing

--         deriving Eq 
--highest precedence -> integers, floats, chars, lists, variables, let, if then else, lambda


-- makes a free var from a string
mkFreeVar :: String -> Ast
mkFreeVar = Var

-- applies the 2nd term to the first
mkApp  ::  Ast -> Ast -> Ast
mkApp = App

-- creates a lambda out of a term and the name of a free var
bindToLam :: String -> Ast -> Ast
bindToLam = Lam

--instance Show Ast where
--  -- display the ast in a readable way
  --show ast = showPretty ast 0

-- | output the fully parenthesized statement
showFullyParen :: Ast  -- ^ The Ast to show
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (DotMixIn a b) = "(" ++ (showFullyParen a) ++ " . " ++ (showFullyParen b) ++ ")"
--showFullyParen (Letrec a b c) = "(letrec" ++ a ++ " = " ++ (showFullyParen b) ++ " in " ++ (showFullyParen c) ++ ")"  
showFullyParen (Concat a b) = "(" ++ (showFullyParen a) ++ " ++ " ++ (showFullyParen b) ++ ")"
showFullyParen (DivFloat a b) = "(" ++ (showFullyParen a) ++ " / " ++ (showFullyParen b) ++ ")"
showFullyParen (Modulus a b) = "(" ++ (showFullyParen a) ++ " % " ++ (showFullyParen b) ++ ")"
showFullyParen (FloatExp a b) = "(" ++ (showFullyParen a) ++ " ^ " ++ (showFullyParen b) ++ ")"
showFullyParen (IntExp a b) = "(" ++ (showFullyParen a) ++ " ** " ++ (showFullyParen b) ++ ")"
showFullyParen (ListIndex a b) =  "(" ++ (showFullyParen a) ++ " !! " ++ (showFullyParen b) ++ ")"
showFullyParen (Print b)         = "print(" ++ showFullyParen b ++ ")"
showFullyParen (Separator a b) = "(" ++ (showFullyParen a) ++ " ; " ++ (showFullyParen b) ++ ")"
showFullyParen (Equal a b) = "(" ++ (showFullyParen a) ++ " == " ++ (showFullyParen b) ++ ")"
showFullyParen (NotEqual a b) = "(" ++ (showFullyParen a) ++ " /= " ++ (showFullyParen b) ++ ")"
showFullyParen (LessThan a b) = "(" ++ (showFullyParen a) ++ " < " ++ (showFullyParen b) ++ ")"
showFullyParen (LessThanOrEqual a b) = "(" ++ (showFullyParen a) ++ " <= " ++ (showFullyParen b) ++ ")"
showFullyParen (GreaterThan a b) = "(" ++ (showFullyParen a) ++ " > " ++ (showFullyParen b) ++ ")"
showFullyParen (GreatThanOrEqual a b) = "(" ++ (showFullyParen a) ++ " >= " ++ (showFullyParen b) ++ ")"
showFullyParen (ValFloat i) = "(" ++ show i ++ ")"
showFullyParen (ValChar i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"
-- | provide a nice show with minimal parentheses

--NEED TO CHECK PRECEDENCE AND LEFT RIGHT ASSOCIATIVITY

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty (ValInt i) _              =  if i < 0
                                        then  "(" ++ show i ++ ")"
                                        else show i
showPretty (ValBool True) _          = "true"
showPretty (ValBool False)  _        = "false"
showPretty (ValFloat i) _            = if i < 0
                                       then  "(" ++ show i ++ ")"
                                       else show i
showPretty (ValChar c) _             = show c
showPretty (Let v a bod) i           = parenthesize 1 i  $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i              = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (Lam v bod) i             = parenthesize 1 i  $ "\\ " ++ v ++ " -> " ++ (showPretty bod 100)
--showPretty (Letrec v a bod) i        = parenthesize 1 i  $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (DotMixIn a b) i          = parenthesize 20 i $ (showPretty a 20) ++ " . " ++ (showPretty b 21)  -- L same level as list indexx

showPretty Nil _                     = "[]"
showPretty (Var s) _                 = s
showPretty (Not l ) i                = parenthesize 23 i $  " ! " ++ (showPretty l 24)
showPretty (Print b)          _      = "print(" ++ showPretty b 23 ++ ")"  

showPretty (ListIndex l r) d         = parenthesize 20 d $ ((showPretty l 20) ++ " !! " ++ (showPretty r 21))

showPretty (FloatExp l r) i          = parenthesize 18 i $ (showPretty l 19) ++ " ^ " ++ (showPretty r 18) --R
showPretty (IntExp l r) i            = parenthesize 18 i $ (showPretty l 19) ++ " ** " ++ (showPretty r 18) --R


showPretty (Mult l r) i              = parenthesize 16 i $ (showPretty l 16) ++ " * " ++ (showPretty r 17)
showPretty (Div l r) i               = parenthesize 16 i $ (showPretty l 16) ++ " // " ++ (showPretty r 17)
showPretty (Modulus l r) i           = parenthesize 16 i $ (showPretty l 16) ++ " % " ++ (showPretty r 17)
showPretty (DivFloat l r) i          = parenthesize 16 i $ (showPretty l 16) ++ " / " ++ (showPretty r 17)

showPretty (Minus l r) i             = parenthesize 14 i $ (showPretty l 14) ++ " - " ++ (showPretty r 15)
showPretty (Plus l r) i              = parenthesize 14 i $ (showPretty l 14) ++ " + " ++ (showPretty r 15)


showPretty (l `Concat` r) d          = parenthesize 12 d $ (showPretty l 13) ++ " ++ " ++ (showPretty r 12) --R
showPretty (Cons l r) i              = parenthesize 12 i $ (showPretty l 13) ++ " : " ++ (showPretty r 12) --R

showPretty (l `LessThan` r) i        = parenthesize 10 i $ (showPretty l 10) ++ " < " ++ (showPretty r 11)
showPretty (l `Equal` r) i           = parenthesize 10 i $ (showPretty l 10) ++ " == " ++ (showPretty r 11)
showPretty (l `NotEqual` r) i        = parenthesize 10 i $ (showPretty l 10) ++ " /= " ++ (showPretty r 11)
showPretty (l `GreaterThan` r) i     = parenthesize 10 i $ (showPretty l 10) ++ " > " ++ (showPretty r 11)
showPretty (l `LessThanOrEqual` r) i = parenthesize 10 i $ (showPretty l 10) ++ " <= " ++ (showPretty r 11)
showPretty (l `GreatThanOrEqual` r)i = parenthesize 10 i $ (showPretty l 10) ++ " >= " ++ (showPretty r 11)


showPretty (And l r) i               = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)

--check list index
showPretty (Or l r) i                = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)

showPretty (App l r) i               = parenthesize 4 i $ (showPretty l 4) ++ " " ++ (showPretty r 5)

showPretty (l `Separator` r) d       = parenthesize 2 d ((showPretty l 3) ++ " ; " ++  (showPretty r 2) ) -- binds most weakly R!

