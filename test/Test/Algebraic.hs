module Test.Algebraic (
    tests
) where

import Expr
import Simplification.Algebraic

import Test.HUnit

x :: Expr
x = symbol "x"

y :: Expr
y = symbol "y"

z :: Expr
z = symbol "z"

te1 :: Test
te1 = TestCase $ assertEqual "expand (x+1)^2" (expand ((x + 1)**2))  (x**2 + 2*x + 1)

te2 :: Test
te2 = TestCase $ assertEqual "expand (x+1)^3" (expand ((x + 1)**3))  (x**3 + 3*x**2 + 3*x + 1)

te3 :: Test
te3 = TestCase $ assertEqual "expand (x+1)^4" (expand ((x + 1)**4))  (x**4 + 4*x**3 + 6*x**2 + 4*x + 1)

te4 :: Test
te4 = TestCase $ assertEqual "expand (x+2)(x-3)" (expand ((x+2)*(x-3)))  (x**2 - x - 6)

te5 :: Test
te5 = TestCase $ assertEqual "expand (x+2)(x-3)(x+1)" (expand ((x+2)*(x-3)*(x+1)))  (x**3 - 7*x - 6)

te6 :: Test
te6 = TestCase $ assertEqual "expand (x+1)(x-2) - (x-1)*x" (expand ((x + 1)*(x - 2) - (x - 1)*x))  (-2)

te7 :: Test
te7 = TestCase $ assertEqual "expand (x+2)(x+3)(x+4)" (expand ((x+2)*(x+3)*(x+4)))  (x**3+9*x**2+26*x+24)

te8 :: Test
te8 = TestCase $ assertEqual "expand (x+y+z)**3" (expand ((x+y+z)**3)) (x**3 + 3*x**2*y + 3*x**2*z + 3*x*y**2 + 6*x*y*z + 3*x*z**2 + y**3 + 3*y**2*z + 3*y*z**2 + z**3)

te9 :: Test
te9 = TestCase $ assertEqual "expand (x+1)^2 + (y+1)^2" (expand ((x+1)**2 + (y+1)**2)) (x**2 + 2*x + y**2 + 2*y + 2)

te10 :: Test
te10 = TestCase $ assertEqual "expand ((x+2)^2 + 3)^2" (expand (((x+2)**2 + 3)**2)) (x**4 + 8*x**3 + 30*x**2 + 56*x + 49)

tests :: Test
tests = TestLabel "Expansion algebraica" $ TestList [te1, te2, te3, te4, te5, te6, te7, te8, te9, te10]