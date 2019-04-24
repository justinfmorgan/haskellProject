module Ast where

-- | the abstract syntax tree for the language
data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast
         
         | ValFloat Float -- added
         | ValChar Char -- added
         | List [Ast] -- added

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
           deriving Eq -- helpful to use this during testing

--         deriving Eq 
--highest precedence -> integers, floats, chars, lists, variables, let, if then else, lambda


instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0

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
showFullyParen (LessThanOrEqual a b) = "(" ++ (showFullyParen a) ++ " <= " ++ (showFullyParen b) ")""
showFullyParen (GreaterThan a b) = "(" ++ (showFullyParen a) ++ " > " ++ (showFullyParen b) ++ ")"
showFullyParen (GreatThanOrEqual a b) = "(" ++ (showFullyParen a) ++ " >= " ++ (showFullyParen b) ")"
showFullyParen (ValFloat i) = "(" ++ i ++ ")"
showFullyParen (ValChar i) = "(" ++ i ++ ")"
showFullyParen (List i) = "(" ++ "[" ++ (showFullyParen i) ++ "]" ++ ")"
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
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty = undefined
