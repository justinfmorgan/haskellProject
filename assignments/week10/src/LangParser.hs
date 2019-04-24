module LangParser where

import Lang
import ParserMonad
import EnvUnsafe

t1 = "5"
t2 = "thisIsAnIdentifier"
t3 = "true"
t4 = "false"
t5 = "[]"
t6 = "[   ]"
t7 = "! false"
t8 = "!5"
t9 = "! ! ! []"
t10 = "4 * 2"
t11 = "2 / 1"
t12 = "3 * 9 / value  * 4"
t13 = "!4 * true / ! ! []"
t14 = "2 + 4"
t15 = "9 - 2"
t16 = "2 * x - 2 / 1"
t17 = "! 6 * [] - true"
t18 = "true && b "
t19 = "false || true"
t20 = "false && true || false && false"
t21 = "! false && ! boolIdent || ! true"
t22 = "4 && ![] || false"
t23 = " 4 : []"
t24 = "true : false : x : []"
t25 = "4 + 2 : 5 / 1 : ! false && true || z : []"
t26 = "f 5"
t27 = "f g h 3"
t28 = " f g 8 * 2"
t29 = "f 3 : []"
t30 = "if true then 2 else 6"
t31 = "if true && false then 2 + 5 else 9 : []"
t32 = "if true then 3 else if false then 9 else 1"
t33 = "let x = 5 in x"
t34 = "let x = 5 * 7 in x : []"
t35 = "let x = true : [] in let y = 5 in y : x"
t36 = "let y = if 3 then 2 else true in let y = if true then 5 else [] in let x = 4 in x"
t37 = "\\x -> 5"
t38 = "\\x -> \\y -> x"
t39 = "\\x -> let y = x in x && y"
t40 = "\\x -> if x then true else y : []"
t41 = "\\x -> x y"
t42 = "\\x -> x : y"
t43 = "if \\x -> x then \\y -> y else \\z -> z"
t44 = "let f = \\x -> x y in \\z -> f z x"
t45 = "\\x -> x + 4 : []"
t46 = "4 * (2 + 9)"
t47 = "f (\\x -> x y) ( (4 : 3) x ) "
t48 = "f (g x) (h 4)"
t49 = "3 + if true then 5 else 9"
t50 = "if true then 5 else 9 + 3"
t51 = "(if true then 5 else 9) + 3"
t52 = "2 : \\x -> x : 1"
t53 = "8 - let x = 2 in 1 - 3"

-- These are the "weird" examples from the distribution file
t54 = "a * (b || c)"
t55 = "y - ((true (-3)) - false * false)"
t56 = "z :  !  ! ((-18) * [] +  ! 2)"
t57 =  "! ! (-18)"

bigone = "Or (Mult (Lam z (Cons (App (If (ValBool True) (ValBool False) (ValInt (-27))) (And Nil Nil)) (Div (And (ValBool False) Nil) (Div Nil (ValBool False))))) (App (App (Div (Not Nil) (Minus Nil (ValInt 12))) (If (ValInt 14) (ValInt (-23)) Nil)) (App (Cons (Minus (ValBool True) Nil) (Var x)) (App (Plus (ValInt 12) Nil) (If Nil (ValInt (-28)) (ValInt (-10))))))) (Div (Div (Plus (Let z (Cons Nil (ValInt 41)) (Var y)) (Var x)) (Minus (Let z (Div (ValInt (-40)) (ValInt 12)) (Minus Nil Nil)) (Mult (Let x Nil (ValBool True)) (Or Nil (ValBool True))))) (And (Cons (Or (Cons (ValInt 3) (ValInt 8)) (Plus Nil (ValBool True))) (Cons (Cons (ValInt 38) (ValInt 31)) (Cons (ValBool False) (ValInt (-17))))) (If (Minus (ValInt (-11)) (ValInt (-2))) (Div (ValInt 43) Nil) (Lam x Nil))))"

p = parse parser

p' x = case parse parser x of
          Just (res,"") -> showPretty res 0
          Just (res,_)  -> "Partial parse: " ++ showPretty res 0 
          Nothing       -> "Parsing Error"

test x = do print x
            putStrLn $ show $ p x
            putStrLn $ p' x
            putStrLn ""



keywords = ["if","then","else", "let", "in", "true","false"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s

--bools :: Parser Ast
--bools = do s <- token $ (literal "true") | (literal "false")
  --         return 

bools :: Parser Ast
bools = 
  do s <- token ((literal "true") <||> (literal "false"))
     let res = case s of
                 Left _  ->  ValBool True
                 Right _ ->  ValBool False
     return res -- keep doing this as much as possible


nil :: Parser Ast
nil = do s <- token (literal "[")
         a <- spaces
         b <- token (literal "]")
         return Nil

apps :: Parser Ast
apps = withInfix beforeApps [("",App)] -- the tokens eat up all the spaces so we split on the empty string

--apps:: Parser Ast
--apps = do l <- beforeApps
  --        parseApp l

parseApp:: Ast -> Parser Ast --like multiplication 
parseApp left = do s <- token (literal "")
                   right <- beforeApps
                   let res = (App left right)
                   parseApp res <|> return res  

beforeApps:: Parser Ast
beforeApps = consEpr <|> orExpr --beforeCons

consEpr:: Parser Ast
consEpr = do a <- orExpr
             (do token (literal ":")
                 c <- consEpr
                 return (Cons a c)) <|> (return a)

 --right associate don't use withInfix!!! withInfix beforeCons [(":",Cons)]

-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")

beforeCons:: Parser Ast
beforeCons = orExpr <|> beforeOr

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

--orExpr = do a <- orExpr
  --          b <- token (literal "||")
    --        c <- token (literal ":")
      --      if (c == ":") then consEpr else return a

-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

beforeOr:: Parser Ast
beforeOr = andExpr <|> beforeAnd

andExpr :: Parser Ast
andExpr = withInfix beforeAnd [("&&", And)] -- addSubExpr before

beforeAnd:: Parser Ast
beforeAnd = addSubExpr <|> beforeAddSub

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSubExpr :: Parser Ast
addSubExpr = withInfix beforeAddSub [("+", Plus), ("-", Minus)]

beforeAddSub:: Parser Ast
beforeAddSub = multDivExpr <|> beforeMult

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = withInfix beforeMult [("*", Mult), ("/", Div)]

beforeMult:: Parser Ast
beforeMult = notExp <|> atoms

notExp :: Parser Ast
notExp = do s <- token $ (literal "!")
            a <- beforeMult
            return (Not a)

atoms:: Parser Ast
atoms = ints <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

ifParser :: Parser Ast
ifParser = do s <- token $ (literal "if")
              a <- parser
              b <- token $ (literal "then")
              c <- parser
              d <- token $ (literal "else")
              e <- parser
              return $ If a c e

--letParser :: Parser Ast
--letParser = undefined

letParser:: Parser Ast
letParser = do token $ literal "let"
               name <- varParser
               token $ literal "="
               yee <- parser --in real life this would be an Ast parser
               token $ literal "in"
               body <- parser --in real life this would be an Ast parser
               --let final = (name, nat, body)
               return (Let name yee body)

-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")


--lambdaParser :: Parser Ast
--lambdaParser = undefined

lambdaParser:: Parser Ast --working correctly!
lambdaParser = do token $ (literal "\\")
                  s <- varParser
                  token $ (literal "->")
                  t <- parser
                  return (Lam s t) 

parens :: Parser Ast
parens = do token $ literal "("
            a <- parser
            token $ literal ")"
            return a

parser :: Parser Ast
parser = apps -- <|> consEpr <|> orExpr <|> andExpr <|> addSubExpr <|> multDivExpr <|> notExp <|> atoms

-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]



-- for repl testing
data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     Ok v -> Result v
                     Error e -> RuntimeError e
  _  -> ParseError