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

sep:: Ast -> Parser Ast --lowest in precedence          
sep left  = do s <- (token $ literal ";")
               exp <- apps
               let res = left `Separator` exp
               (sep res) <|> return res

sepEpr:: Parser Ast
sepEpr = do l <- apps
            sep l

ignoreComments:: Parser ()
ignoreComments = do token $ literal "--" 
                    return ()

remove :: Parser ()
remove =  do token (literal "{-")
             rep (sat isSpace)
             rep (sat isDigit)
             rep (sat isAlpha)
             token (literal "-}")
             return ()


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
apps = withInfix beforeApps [("",App)] -- must get beforeapps before or it'll break

parseApp:: Ast -> Parser Ast --like multiplication 
parseApp left = do s <- token (literal "")
                   right <- beforeApps
                   let res = (App left right)
                   parseApp res <|> return res  

beforeApps:: Parser Ast
beforeApps = orExpr <|> andExpr <|> equalities <|> concatEpr <|> bothListTypes -- don't mess with this!!

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
andExpr = withInfix beforeAnd [("&&", And)]

beforeAnd:: Parser Ast
beforeAnd = equalities <|> concatEpr <|> bothListTypes <|> addSubExpr <|> multDivExpr --don't mess with what goes into and

equalities:: Parser Ast             
equalities = withInfix beforeeq [("<=", LessThanOrEqual), ("<", LessThan), (">=", GreatThanOrEqual), (">", GreaterThan), ("/=", NotEqual), ("==", Equal)] 

beforeeq:: Parser Ast
beforeeq = concatEpr <|> bothListTypes <|> addSubExpr <|> beforeAdd -- <|> multDivExpr

concatEpr:: Parser Ast
concatEpr = do a <- bothListTypes 
               b <- token (literal "++")
               c <- bothListTypes
               return (Concat a c)
      
bothListTypes:: Parser Ast
bothListTypes = both <|> addSubExpr <|> multDivExpr -- <|> beforeMult <|> listIndex <|> beforeLI

both:: Parser Ast
both = do a <- token (literal "[") <||> token addAndMore 
          case a of
            Left _ -> do b <- token commas
                         return b
            Right d -> (do token (literal ":")
                           c <- andMore
                           return (Cons d c)) <|> (return d)
andMore:: Parser Ast
andMore = both <|> addSubExpr <|> multDivExpr <|> expEpr <|> listIndex <|> pri <|> notExp <|> atoms

commas:: Parser Ast
commas = do a <- token addSubExpr       --FIXME issues parsing!
            b <- token ((literal ",") <||>  (literal "]"))
            case b of
                Left _ -> do b <- commas
                             return (Cons a b)
                Right _-> return (Cons a Nil)
{-            
consCommmas:: Parser Ast 
consCommmas = do token (literal "[")
                 a <- token commas
                 return a
-}
addAndMore:: Parser Ast
addAndMore = addSubExpr <|> beforeAdd
addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("- ", Minus)] --overloaded for floats and ints

beforeAdd:: Parser Ast
beforeAdd = multDivExpr <|> beforeMult

multDivExpr :: Parser Ast
multDivExpr = withInfix beforeMult [("*", Mult), ("//", Div), ("%", Modulus), ("/", DivFloat)]    --div for ints

beforeMult:: Parser Ast
beforeMult = expEpr <|> listIndex

expEpr:: Parser Ast
expEpr = do a <- listIndex
            (do b <- token $ (literal "^") <||> (literal "**") 
                c <- expEpr
                case b of
                  Left _  -> return (FloatExp a c)
                  Right _ -> return (IntExp a c)
                  ) <|> (return a)

{-
floatExpEpr:: Parser Ast -- symbol ^ R associative
floatExpEpr = do a <- intExpEpr              
                 (do token (literal "^")
                     c <- floatExpEpr
                     return (FloatExp a c)) <|> (return a) 

intExpEpr:: Parser Ast  -- symbol ** R associate
intExpEpr = do a <- listIndex      
               (do token (literal "**")
                   c <- intExpEpr
                   return (IntExp a c)) <|> (return a)      
-}
beforeLI:: Parser Ast
beforeLI = pri <|> notExp <|> atoms

listIndex:: Parser Ast         
listIndex = withInfix beforeLI [("!!", ListIndex)]

pri:: Parser Ast
pri = do token $ literal "print"        --should it get parser or atoms???? FIXME
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
atoms = pri <|> parseChar <|> parseFloat <|> ints <|> bools  <|>  nil <|> parens <|> ifParser <|> letrecParser <|> letParser <|> {- dotmixinParser <|>-} lambdaParser <|> vars

parseChar:: Parser Ast                      --FIXME needs to work for just a not a space !
parseChar = do s <- token (literal "'")
               a <- item --sat isAlpha
               --b <- literal " "
               b <- token (literal "'")
               return (ValChar a)

parseFloat:: Parser Ast
parseFloat = do a <- floatParserActual
                return (ValFloat a)

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
  do s <- token ((literal "True") <||> (literal "true") <||> (literal "False") <||> (literal "false"))
     let res = case s of
                 Left (Right _) -> ValBool False
                 Left _  ->  ValBool True

                 Right _ ->  ValBool False
     return res -- keep doing this as much as possible


nil :: Parser Ast
nil = do s <- token (literal "[")
         a <- spaces
         b <- token (literal "]")
         return Nil

ifParser :: Parser Ast
ifParser = do s <- token $ (literal "if")
              a <- parser
              b <- token $ (literal "then")
              c <- parser
              d <- token $ (literal "else")
              e <- parser
              return $ If a c e

letParser:: Parser Ast
letParser = do token $ literal "let"
               name <- varParser
               token $ literal "="
               yee <- parser --in real life this would be an Ast parser
               token $ literal "in"
               body <- parser --in real life this would be an Ast parser
               --let final = (name, nat, body)
               return (Let name yee body)

letrecParser:: Parser Ast
letrecParser = do token $ literal "letrec"
                  name <- varParser
                  token $ literal "="
                  yee <- parser --in real life this would be an Ast parser
                  token $ literal "in"
                  body <- parser --in real life this would be an Ast parser
               --let final = (name, nat, body)
                  return (Letrec name yee body)

lambdaParser:: Parser Ast --working correctly!
lambdaParser = do token $ (literal "\\")
                  s <- varParser
                  token $ (literal "->")
                  t <- parser
                  return (Lam s t) 
{-
dotmixinParser:: Parser Ast
dotmixinParser = do a <- parser
                    token $ literal "."
                    b <- parser
                    return (DotMixIn a b
                      )
dotmixinParser = withInfix parser [(".", DotMixIn)]

-}
parens :: Parser Ast
parens = do token $ literal "("
            a <- parser
            token $ literal ")"
            return a

parser :: Parser Ast
parser = sepEpr <|> apps <|> orExpr <|> andExpr <|> equalities <|> concatEpr <|> bothListTypes <|> beforeMult <|> listIndex <|> beforeLI