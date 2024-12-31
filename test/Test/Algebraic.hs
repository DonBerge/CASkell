module Test.Algebraic (
    tests
) where

import Expr
import Simplification.Algebraic

import Test.HUnit

x :: Expr
x = symbol "x"

te1 :: Test
te1 = TestCase $ assertEqual "expand (x + 1)1 == x + 1" (expand ((x + 1)**2))  (x**2 + 2*x + 1)

te2 :: Test
te2 = TestCase $ assertEqual "expand (x + 1)2 == x + 1" (expand ((x + 1)**3))  (x**3 + 3*x**2 + 3*x + 1)

te3 :: Test
te3 = TestCase $ assertEqual "expand (x + 1)3 == x + 1" (expand ((x + 1)**4))  (x**4 + 4*x**3 + 6*x**2 + 4*x + 1)

te4 :: Test
te4 = TestCase $ assertEqual "expand (x + 1)4 == x + 1" (expand ((x+2)*(x-3)))  (x**2 - x - 6)

te5 :: Test
te5 = TestCase $ assertEqual "expand (x + 1)5 == x + 1" (expand ((x+2)*(x-3)*(x+1)))  (x**3 - 7*x - 6)

te6 :: Test
te6 = TestCase $ assertEqual "expand (x + 1) 6== x + 1" (expand ((x + 1)*(x - 2) - (x - 1)*x))  (-2)

tests :: Test
tests = TestLabel "Expansion algebraica" $ TestList [te1, te2, te3, te4, te5, te6]