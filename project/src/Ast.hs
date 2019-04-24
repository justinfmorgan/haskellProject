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
showPretty = undefined
