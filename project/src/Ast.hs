module Ast where

import HelpShow

-- | the abstract syntax tree for the language
data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast
         
         | ValFloat Float -- added
         | ValChar Char -- added
--         | List [Ast] -- added

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Separator Ast Ast  --- all added
         | Equal Ast Ast | NotEqual Ast Ast
         | LessThan Ast Ast | LessThanOrEqual Ast Ast 
         | GreaterThan Ast Ast | GreatThanOrEqual Ast Ast
         | Concat Ast Ast
         | DivFloat Ast Ast 
         | Modulus Ast Ast -- only for integers
         | FloatExp Ast Ast
         | IntExp Ast Ast
         | ListIndex Ast Ast -- left -> list, right -> integer
         | Print Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
           deriving (Show,Eq) -- helpful to use this during testing

--         deriving Eq 
--highest precedence -> integers, floats, chars, lists, variables, let, if then else, lambda


--instance Show Ast where
--  -- display the ast in a readable way
  --show ast = showPretty ast 0

-- | output the fully parenthesized statement
showFullyParen :: Ast  -- ^ The Ast to show
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
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
showPretty (ValChar c) _             = c
showPretty (Let v a bod) i           = parenthesize 1 i  $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i              = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (Lam v bod) i             = parenthesize 1 i  $ "\\ " ++ v ++ " -> " ++ (showPretty bod 100)

--showPretty (List [x]) _              = undefined
showPretty Nil _                     = "[]"
showPretty (Var s) _                 = s
showPretty (Not l ) i                = parenthesize 23 i $  " ! " ++ (showPretty l 23)
showPretty (Print b)          _      = "print(" ++ showPretty b 22 ++ ")"  

showPretty (ListIndex l r) d         = parenthesize 20 d $ ((showPretty l 20) ++ " !! " ++ (showPretty r 21))

showPretty (FloatExp l r) i          = parenthesize 18 i $ (showPretty l 19) ++ " ** " ++ (showPretty r 18) --R
showPretty (IntExp l r) i            = parenthesize 18 i $ (showPretty l 19) ++ " ^ " ++ (showPretty r 18) --R


showPretty (Mult l r) i              = parenthesize 16 i $ (showPretty l 16) ++ " * " ++ (showPretty r 17)
showPretty (Div l r) i               = parenthesize 16 i $ (showPretty l 16) ++ " // " ++ (showPretty r 17)
showPretty (Modulus l r) i           = parenthesize 16 i $ (showPretty l 16) ++ " % " ++ (showPretty r 17)
showPretty (DivFloat l r) i          = parenthesize 16 i $ (showPretty l 16) ++ " / " ++ (showPretty r 17)

showPretty (Minus l r) i             = parenthesize 14 i $ (showPretty l 14) ++ " - " ++ (showPretty r 15)
showPretty (Plus l r) i              = parenthesize 14 i $ (showPretty l 14) ++ " + " ++ (showPretty r 15)


showPretty (l `Concat` r) d          = parenthesize 12 d $ (showPretty l 13) ++ " ++ " ++ (showPretty r 12) --R
showPretty (Cons l r) i              = parenthesize 12 i $ (showPretty l 13) ++ " : " ++ (showPretty r 12) --R

showPretty (l `LessThan` r) i        = parenthesize 10 i $ (showPretty l 11) ++ " < " ++ (showPretty r 11)
showPretty (l `Equal` r) i           = parenthesize 10 i $ (showPretty l 11) ++ " == " ++ (showPretty r 11)
showPretty (l `NotEqual` r) i        = parenthesize 10 i $ (showPretty l 11) ++ " /= " ++ (showPretty r 11)
showPretty (l `GreaterThan` r) i     = parenthesize 10 i $ (showPretty l 11) ++ " > " ++ (showPretty r 11)
showPretty (l `LessThanOrEqual` r) i = parenthesize 10 i $ (showPretty l 11) ++ " >= " ++ (showPretty r 11)
showPretty (l `GreatThanOrEqual` r)i = parenthesize 10 i $ (showPretty l 11) ++ " <= " ++ (showPretty r 11)


showPretty (And l r) i               = parenthesize 8 i $ (showPretty l 9) ++ " && " ++ (showPretty r 9)

--check list index
showPretty (Or l r) i                = parenthesize 6 i $ (showPretty l 7) ++ " || " ++ (showPretty r 7)

showPretty (App l r) i               = parenthesize 4 i $ (showPretty l 4) ++ " " ++ (showPretty r 5)

showPretty (l `Separator` r) d       = parenthesize 2 d ((showPretty l 3) ++ " ; " ++  (showPretty r 2) ) -- binds most weakly R!

