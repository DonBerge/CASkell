module Test.Derivate (
    tests
) where

import Test.HUnit
import Expr

import Calculus

x = symbol "x"
y = symbol "y"
z = symbol "z"

derivt1 :: Test
derivt1 = TestCase $ assertEqual "derivate: cos x" (-sin x) (derivate (cos x) x)

derivt2 :: Test
derivt2 = TestCase $ assertEqual "derivate: e^(x^2)" (2*x*exp(x**2)) (derivate (exp (x**2)) x)

derivt3 :: Test
derivt3 = TestCase $ assertEqual "derivate 3 times: x**4" (24*x) (derivateMultiple (x**4) x 3)

derivt4 :: Test
derivt4 = TestCase $ assertEqual "derivate: x*sin(x**2)+1" (2 * x**2 * cos(x**2) + sin(x**2)) (derivate (x*sin(x**2)+1) x)

derivt5 :: Test
derivt5 = TestCase $ assertEqual "derivate: x**2 - y**3 + 2*y" (2*x) (derivate (x**2 + y**3 + 2*y) x)

derivt6 :: Test
derivt6 = TestCase $ assertEqual "derivate: sin(x) * cos(x)" (cos(x)**2 - sin(x)**2) (derivate (sin(x) * cos(x)) x)

derivt7 :: Test
derivt7 = TestCase $ assertEqual "derivate: ln(x)" (1/x) (derivate (log(x)) x)

derivt8 :: Test
derivt8 = TestCase $ assertEqual "derivate: x^3 + 3*x^2 + 3*x + 1" (3*x**2 + 6*x + 3) (derivate (x**3 + 3*x**2 + 3*x + 1) x)

derivt9 :: Test
derivt9 = TestCase $ assertEqual "derivate: x^5 - 5*x^3 + 5*x" (5*x**4 - 15*x**2 + 5) (derivate (x**5 - 5*x**3 + 5*x) x)

derivt10 :: Test
derivt10 = TestCase $ assertEqual "derivate: sin(x) / cos(x)" (1 + sin(x)**2 / cos(x)**2) (derivate (sin(x) / cos(x)) x)

derivt11 :: Test
derivt11 = TestCase $ assertEqual "derivate: x^3 * sin(x)" (3*x**2 * sin x + x**3 * cos(x)) (derivate (x**3 * sin(x)) x)

derivt12 :: Test
derivt12 = TestCase $ assertEqual "derivate: x^2 * e^x" (2*x * exp x + x**2 * exp x) (derivate (x**2 * exp x) x)

derivt13 :: Test
derivt13 = TestCase $ assertEqual "derivate: ln(x^2 + 1)" (2*x / (x**2 + 1)) (derivate (log(x**2 + 1)) x)

derivt14 :: Test
derivt14 = TestCase $ assertEqual "derivate: 3/2 * y * sqrt x" (3/2 * y * sqrt x) (derivate (x*y*sqrt x) x)

derivt15 :: Test
derivt15 = TestCase $ assertEqual "derivateMultiple 2 times: x^4" (12*x**2) (derivateMultiple (x**4) x 2)

derivt16 :: Test
derivt16 = TestCase $ assertEqual "derivateMultiple 4 times: x^5" (120*x) (derivateMultiple (x**5) x 4)

derivt17 :: Test
derivt17 = TestCase $ assertEqual "derivateMultiple 3 times: sin(x)" (-sin(x)) (derivateMultiple (sin(x)) x 2)

derivt18 :: Test
derivt18 = TestCase $ assertEqual "derivateMultiple 2 times: cos(x)" (-cos(x)) (derivateMultiple (cos(x)) x 2)

derivt19 :: Test
derivt19 = TestCase $ assertEqual "derivateMultiple 3 times: e^x" (exp(x)) (derivateMultiple (exp(x)) x 3)

derivt20 :: Test
derivt20 = TestCase $ assertEqual "derivateMultiple 2 times: ln(x)" (-1 / x**2) (derivateMultiple (log(x)) x 2)

tests :: Test
tests = TestList [derivt1, derivt2, derivt3, derivt4, derivt5, derivt6, derivt7, derivt8, derivt9, derivt10, derivt11, derivt12, derivt13, derivt14, derivt15, derivt16, derivt17, derivt18, derivt19, derivt20]