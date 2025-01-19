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

---

cont u x r = do
              u' <- u
              x' <- x
              r' <- sequence r
              polyContent u' x' r'

contt1 :: Test
contt1 = TestCase $ assertEqual "content: x**2+2*x*1" 1 (cont (x**2 + 2*x + 1) x [])

contt2 :: Test
contt2 = TestCase $ assertEqual "content: (1/2)*x*y+6*y)" y (cont ((1/2)*x*y+6*y) x [y])

contt3 :: Test
contt3 = TestCase $ assertEqual "content: -y*x**2+y**3" y (cont (-y*x**2+y**3) x [y])

contt4 :: Test
contt4 = TestCase $ assertEqual "content: -y*x**2+y**3" y (cont (y*x**2+2*y**2*x+y**3) x [y])

pgcdt1 :: Test
pgcdt1 = TestCase $ assertEqual "pgcd: x**2+2*x+1 and x+1" (x+y) (polGCD (x**2+2*x*y+y**2) (x+y) [x,y])

pgcdt2 :: Test
pgcdt2 = TestCase $ assertEqual "pgcd: x**2+2*x+1 and x+1" (x*y+y**2) (polGCD (-y*x**2 + y**3) (y*x**2 + 2 * y**2 * x + y**3) [x,y])

pgcdt3 :: Test
pgcdt3 = TestCase $ assertEqual "pgcd: x**2+2*x+1 and x+1" (x*y) (polGCD (-4*x**3*y+4*x*y**3+4*x*y) (6*x**4*y+12*x**3*y**2+6*y**3*x**2) [x,y])

pgcdt4 :: Test
pgcdt4 = TestCase $ assertEqual "pgcd: x**2+2*x+1 and x+1" (x-y) (polGCD (-4*x**2+4*y**2) (8*x**2-16*x*y+8*y**2) [x,y])

---
rs1 :: Test
rs1 = TestCase $ assertEqual "rational simplify: (-4*x**2+4*y**2)/(8*x**2-16*x*y+8*y**2)" (x+y) (rationalSimplify ((-4*x**2+4*y**2) / (8*x**2-16*x*y+8*y**2)))

tests :: Test
tests = TestList [ pdt1, nt1, nt2, nt3, pgcdt1, pgcdt2, pgcdt3, pgcdt4, contt1, contt2, contt3, contt4, rs1 ]