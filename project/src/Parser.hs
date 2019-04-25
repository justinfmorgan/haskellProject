module Parser where

import Ast
import ParserMonad
import Data.Char --added

{- What must be added into the parser:
         ValFloat Float            ValChar Char         Separator Ast Ast         Equal Ast Ast          NotEqual Ast Ast
         LessThan Ast Ast          LessThanOrEqual Ast Ast          GreaterThan Ast Ast       GreatThanOrEqual Ast Ast
         Concat Ast Ast            DivFloat Ast Ast          Modulus Ast Ast -- only for integers         FloatExp Ast Ast          
         IntExp Ast Ast         ListIndex Ast Ast          Print Ast
		 ignoring comments!!!!!
-}

sep:: Ast -> Parser Ast --lowest in precedence          FIXME (what goes into it?)
sep left  = do s <- (token $ literal ";")
               exp <- apps
               let res = left `Separator` exp
               (sep res) <|> return res

sepEpr:: Parser Ast
sepEpr = do l <- apps
            sep l


ignoreComments:: Parser Ast
ignoreComments = token $ literal "--"

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

bracket = ["["]


apps :: Parser Ast
apps = withInfix orExpr [("",App)] -- TODO does this work??? the tokens eat up all the spaces so we split on the empty string

--apps:: Parser Ast
--apps = do l <- beforeApps
  --        parseApp l

parseApp:: Ast -> Parser Ast --like multiplication 
parseApp left = do s <- token (literal "")
                   right <- beforeApps
                   let res = (App left right)
                   parseApp res <|> return res  

beforeApps:: Parser Ast
beforeApps = orExpr --beforeCons


 --right associate don't use withInfix!!! withInfix beforeCons [(":",Cons)]

-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]


-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

andExpr :: Parser Ast
andExpr = withInfix beforeAnd [("&&", And)] -- can this work with just addSubExpr here?? TODO


-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")


beforeAnd:: Parser Ast
beforeAnd = greatTEpr <|> greatTEqEpr <|> lessTEpr <|> lessTEqEpr <|> eqEpr <|> notEqEpr 

eqEpr:: Parser Ast              
eqEpr = withInfix beforeEqsStuff [("==", Equal)] 

notEqEpr:: Parser Ast          
notEqEpr = withInfix beforeEqsStuff [("/=", NotEqual)] 

greatTEpr:: Parser Ast          
greatTEpr = withInfix beforeEqsStuff [(">", GreaterThan)] 

greatTEqEpr:: Parser Ast        
greatTEqEpr = withInfix beforeEqsStuff [(">=", GreatThanOrEqual)] 

lessTEpr:: Parser Ast          
lessTEpr = withInfix beforeEqsStuff [("<", LessThan)] 

lessTEqEpr:: Parser Ast             
lessTEqEpr = withInfix beforeEqsStuff [("<=", LessThanOrEqual)] 

beforeEqsStuff:: Parser Ast
beforeEqsStuff = concatEpr <|> consEpr <|> addSubExpr

concatEpr:: Parser Ast
concatEpr = do a <- consEpr --FIXME hardcore
               b <- token (literal "++")
               c <- consEpr --FIXME -> figure out precedence
               return (Concat a c)

consEpr:: Parser Ast            --2 ways: a:b:[] [a,b]
consEpr = do a <- token addSubExpr      --TODO doublecheck this
             (do token (literal ":")
                 c <- consEpr
                 return (Cons a c)) <|> (return a)

addSubExpr :: Parser Ast
addSubExpr = withInfix beforeAddSub [("+", Plus), ("-", Minus)] --overloaded for floats and ints

beforeAddSub:: Parser Ast
beforeAddSub = multDivExpr <|> beforeMult

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = withInfix beforeMult [("*", Mult), ("/", Div)]    --div for fp

modEpr:: Parser Ast             -- only for integers FIXME
modEpr = withInfix beforeMult [("%", Modulus)]

divIntEpr:: Parser Ast        --TODO
divIntEpr = beforeMult

beforeMult:: Parser Ast
beforeMult = floatExpEpr <|> intExpEpr

floatExpEpr:: Parser Ast -- symbol ^ R associative
floatExpEpr = do a <- listIndex               --FIXME WHAT GOES INTO IT
                 (do token (literal "^")
                     c <- floatExpEpr
                     return (Cons a c)) <|> (return a) -- FIXME not return (Cons a c)

intExpEpr:: Parser Ast  -- symbol ** R associate
intExpEpr = do a <- listIndex             --FIXME WHAT GOES INTO IT
               (do token (literal "**")
                   c <- intExpEpr
                   return (Cons a c)) <|> (return a)              --FIXME not return (Cons a c) FIXME

beforeExp:: Parser Ast
beforeExp = listIndex <|> beforeLI


beforeLI:: Parser Ast
beforeLI = pri <|> notExp <|> atoms


listIndex:: Parser Ast          --left associative not right!!!!
listIndex = withInfix beforeLI [("!!", ListIndex)]
--listIndex = do a <- consEpr
  --             b <- token (literal "!!")
    --           c <- ints
      --         return (ListIndex a c)

pri:: Parser Ast
pri = do token $ literal "print"        --should it get parser or atoms????
         token $ literal "("
         printed <- parser
         token $ literal ")"
         return printed
         `mapParser` (\ i -> Print i)

notExp :: Parser Ast
notExp = do s <- token $ (literal "!")
            a <- atoms
            return (Not a)

atoms:: Parser Ast
atoms = parseChar {-<|> floatParser-} <|> ints <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

floatParser:: Parser Ast                --TODO
floatParser = undefined

parseChar:: Parser Ast                      --FIXME
parseChar = do --s <- token (literal "'")
               a <- sat isAlpha
               --b <- token (literal "'")
               return (ValChar a)

parseFloat:: Parser Ast  --yee idk
parseFloat = undefined
--parseFloat = do s <- intParser
--                a <- literal (".")
--                b <- natParser
--                return (ValFloat (s)) --TODO
 
vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s


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
parser = sepEpr <|> apps <|> orExpr <|> andExpr <|> greatTEpr <|> greatTEqEpr <|> lessTEpr <|> lessTEqEpr <|> eqEpr <|> notEqEpr <|> beforeEqsStuff