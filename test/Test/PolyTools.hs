{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Test.PolyTools (
  tests  
) where

import Simplification.PolyTools
import Test.HUnit ( assertEqual, Test(..) )

import PExpr
import Expr

x :: Expr
x = symbol "x"

y = symbol "y"

pseudoDivide :: MonadFail m => m PExpr -> m PExpr -> m PExpr -> m (PExpr, PExpr)
pseudoDivide u v x = do
                        u' <- u
                        v' <- v
                        x >>= pseudoDivision u' v'

mkp :: Monad m => m a -> m b -> m (a, b)
mkp u v = do
            u' <- u
            v' <- v
            return (u',v')

--- pseudo division
pdt1 :: Test
pdt1 = TestCase $ assertEqual "pseudo division: (x^2 + 2x + 1)/x" (mkp (y*x+y) 0) (pseudoDivide (x**2*y+2*x*y+y) (x+1) x)

--- normalize

normalize' :: MonadFail m => m PExpr -> [m PExpr] -> m PExpr
normalize' u l = do
                l' <- sequence l
                u' <- u
                normalize u' l'
    
nt1 :: Test
nt1 = TestCase $ assertEqual "normalize: 2x + 3x" (7*x-2*x*y-5+y**2) (normalize' (7*x-2*x*y-5+y**2) [y,x])

nt2 :: Test
nt2 = TestCase $ assertEqual "normalize: 2x + 3x" ((-7/2)*x + x*y + 5/2 -(1/2)*y**2) (normalize' (7*x-2*x*y-5+y**2) [x,y])

nt3 :: Test
nt3 = TestCase $ assertEqual "normalize: 2x + 3x" 1 (normalize' 10 [y,x])

tests :: Test
tests = TestList [ pdt1, nt1, nt2, nt3 ]