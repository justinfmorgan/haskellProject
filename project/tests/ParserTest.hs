module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck    --unknown if needed


import ParserMonad (parse)
import Ast (showFullyParen, showPretty, Ast(..))
import Parser (parser)


--TODO: move the generator to a shared place

instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- elements [0..999]
                                 b <- arbitrary
                                 f <- arbitrary
                                 c <- arbitrary
                                 node <- elements [ValInt i, ValBool b, ValFloat f, ValChar c, Nil]
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     ifast <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, Div l r,
                                                       Separator l r, Equal l r, NotEqual l r, LessThan l r, LessThanOrEqual l r,
                                                       GreaterThan l r, GreatThanOrEqual l r, Concat l r, DivFloat l r,
                                                       Modulus l r, FloatExp l r, IntExp l r, ListIndex l r, Print l r,
                                                       Cons l r,
                                                       ifast,
                                                       Let str l r,
                                                       Var str,
                                                       Lam str l,
                                                       App l r
                                                      ]
                                     return node

-- it would be better if every branch were built like this so the balance would be maintained
arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                        t <- arbitrarySizedAst (m `div` 3)
                        e <- arbitrarySizedAst (m `div` 3)
                        return $ If b t e

sample (arbitrarySizedAst 10)

parserTest = testGroup
      "parser Test"
      [
      testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
        
      testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]
