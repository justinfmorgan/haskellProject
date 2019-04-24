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
showFullyParen = undefined

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)

showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)
