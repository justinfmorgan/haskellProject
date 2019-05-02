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

   shrink (Plus l r) = [l, r] ++ [(Plus l' r') | (l', r') <- shrink (l, r)]
   shrink (Mult l r) = [l, r] ++ [(Mult l' r') | (l', r') <- shrink (l, r)]
   shrink (Div l r) = [l, r] ++ [(Div l' r') | (l', r') <- shrink (l, r)]
   shrink (DivFloat l r) = [l, r] ++ [(DivFloat l' r') | (l', r') <- shrink (l, r)]
   shrink (Modulus l r) = [l, r] ++ [(Modulus l' r') | (l', r') <- shrink (l, r)]
   shrink (App l r) = [l, r] ++ [(App l' r') | (l', r') <- shrink (l, r)]
   shrink (Not l) = [l] ++ [(Not l') | (l') <- shrink (l)]
   shrink (Cons l r) = [l, r] ++ [(Cons l' r') | (l', r') <- shrink (l, r)]
   shrink (ListIndex l r) = [l, r] ++ [(ListIndex l' r') | (l', r') <- shrink (l, r)]
   shrink (IntExp l r) = [l, r] ++ [(IntExp l' r') | (l', r') <- shrink (l, r)]
   shrink (FloatExp l r) = [l, r] ++ [(FloatExp l' r') | (l', r') <- shrink (l, r)]
   shrink (Concat l r) = [l, r] ++ [(Concat l' r') | (l', r') <- shrink (l, r)]
   shrink (Equal l r) = [l, r] ++ [(Equal l' r') | (l', r') <- shrink (l, r)]
   shrink (GreaterThan l r) = [l, r] ++ [(GreaterThan l' r') | (l', r') <- shrink (l, r)]
   shrink (GreatThanOrEqual l r) = [l, r] ++ [(GreatThanOrEqual l' r') | (l', r') <- shrink (l, r)]
   shrink (LessThanOrEqual l r) = [l, r] ++ [(LessThanOrEqual l' r') | (l', r') <- shrink (l, r)]
   shrink (LessThan l r) = [l, r] ++ [(LessThan l' r') | (l', r') <- shrink (l, r)]
   shrink (NotEqual l r) = [l, r] ++ [(NotEqual l' r') | (l', r') <- shrink (l, r)]
   shrink (Separator l r) = [l, r] ++ [(Separator l' r') | (l', r') <- shrink (l, r)]
   shrink (Minus l r) = [l, r] ++ [(Minus l' r') | (l', r') <- shrink (l, r)]
   shrink (And l r) = [l, r] ++ [(And l' r') | (l', r') <- shrink (l, r)]
   shrink (Or l r) = [l, r] ++ [(Or l' r') | (l', r') <- shrink (l, r)]
   shrink (If l c r) = [l, c, r] ++ [(If l' c' r') | (l', c', r') <- shrink (l, c, r)]
   shrink (DotMixIn l r) = [l, r] ++ [(DotMixIn l' r') | (l', r') <- shrink (l, r)]


   shrink _ = []

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- elements [0..999]
                                 b <- arbitrary
                                 f <- elements [1.00003,4.5151, 10.5516166, 0.00000, (-4.4), (-17.2511), (-1455.1515), 15.515]--arbitrary
                                 c <- elements ['a'..'z']
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
                                                       Modulus l r, FloatExp l r, IntExp l r, ListIndex l r, Print l, DotMixIn l r,
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

parserTest = testGroup
      "parser Test"
      [
      testProperty "parse should return the same AST when fully parenthesized" $
                  ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
        
      testProperty "parse should return the same AST when pretty printed" $
                  ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]
