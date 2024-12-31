module Main (main) where

import Test.HUnit

import Expr

x :: Expr
x = symbol "x"

y :: Expr
y = symbol "y"

-----------------------------------

sb1 :: Test
sb1 = TestCase $ assertEqual "eval x+0" x (x + 0)

sb2 :: Test
sb2 = TestCase $ assertEqual "eval x*1" x (x * 1)

sb3 :: Test
sb3 = TestCase $ assertEqual "eval 0*x" (0*x) 0

sb4 :: Test
sb4 = TestCase $ assertEqual "eval x-x" (x-x) 0

sb5 :: Test
sb5 = TestCase $ assertEqual "eval x+x" (x+x) (2*x)

sb6 :: Test
sb6 = TestCase $ assertEqual "eval 2 * (4*x)" (2*(4*x)) (8*x)

sb7 :: Test
sb7 = TestCase $ assertEqual "eval 3*x * 3*x * 3*x" (3*x * 3*x * 3*x) (27 * x**3)

symplifyBasic :: Test
symplifyBasic = TestList [sb1, sb2, sb3, sb4, sb5, sb6, sb7]

-----------------------------------

sc1 :: Test
sc1 = TestCase $ assertEqual "eval 2+3" (((2::Expr) + (3::Expr))) (5::Expr)

sc2 :: Test
sc2 = TestCase $ assertEqual "eval 3*4" (((2::Expr) * (3::Expr))) (6::Expr)

sc3 :: Test
sc3 = TestCase $ assertEqual "eval 2^3" ((2::Expr) ** (3::Expr)) (8::Expr)

symplifyConst :: Test
symplifyConst = TestLabel "Simplififcacion de constantes" $ TestList [sc1, sc2, sc3]

-----------------------------------

sf1 :: Test
sf1 = TestCase $ assertEqual "eval x/x" (x/x) 1

sf2 :: Test
sf2 = TestCase $ assertEqual "eval (2*x)/4" ((2*x)/4) (x/2)

sf3 :: Test
sf3 = TestCase $ assertEqual "eval x/x + y/y" ((x/x) + (y/y)) 2

symplifyFraction :: Test
symplifyFraction = TestLabel "Simplififcacion de fracciones" $ TestList [sf1, sf2, sf3]

------------------------------------

assertNotEqual :: Eq a => String -> a -> a -> Assertion
assertNotEqual msg a b = assertBool msg (a /= b)

sr1 :: Test
sr1 = TestCase $ assertNotEqual "sqrt (x ** 2) != x" (sqrt (x**2)) x

sr2 :: Test
sr2 = TestCase $ assertEqual "(sqrt x) ** 2" ((sqrt x)**2) x

symplifyRadical :: Test
symplifyRadical = TestLabel "Simplififcacion de radicales" $ TestList [sr1, sr2]

main :: IO Counts
main = runTestTT $ TestList $ [symplifyBasic, symplifyConst, symplifyFraction, symplifyRadical]
