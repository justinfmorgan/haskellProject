module Parser where

import Ast
import ParserMonad
import Data.Char --added

{- What must be added into the parser:
         ValFloat Float
         ValChar Char
         List [Ast]
		 ignoring comments
-}

charParser:: Parser Char
charParser = 
  do head <- sat isAlpha
     return head

parseChar:: Parser Ast
parseChar = do s <- token $ charParser
               return (ValChar s)
{-
parseFloat:: Parser Ast  --check this
parseFloat = do s <- intParser
                a <- literal (".")
                b <- natParser
                return (ValFloat (s++a++b))
 -}
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
