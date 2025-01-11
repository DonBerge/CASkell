module Main (main) where

import Test.HUnit

import Expr

import qualified Test.Algebraic as Algebraic
import qualified Test.Fu as Fu

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
sb5 = TestCase $ assertEqual "eval 2*x+5*x" (2*x+5*x) (7*x)

sb6 :: Test
sb6 = TestCase $ assertEqual "eval 2 * (4*x)" (2*(4*x)) (8*x)

sb7 :: Test
sb7 = TestCase $ assertEqual "eval 3*x * 3*x * 3*x" (3*x * 3*x * 3*x) (27 * x**3)

symplifyBasic :: Test
symplifyBasic = TestList [sb1, sb2, sb3, sb4, sb5, sb6, sb7]

-----------------------------------

sc1 :: Test
sc1 = TestCase $ assertEqual "eval 2+3" ((2::Expr) + (3::Expr)) (5::Expr)

sc2 :: Test
sc2 = TestCase $ assertEqual "eval 3*4" ((2::Expr) * (3::Expr)) (6::Expr)

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

stSin1 :: Test
stSin1 = TestCase $ assertEqual "sin(2*pi + x)" (sin x) (sin(2*pi+x))

stSin2 :: Test
stSin2 = TestCase $ assertEqual "sin(pi/2)" 1 (sin((pi::Expr)/2))

stSin3 :: Test
stSin3 = TestCase $ assertEqual "sin(0)" 0 (sin(0 :: Expr))

stSin4 :: Test
stSin4 = TestCase $ assertEqual "sin(-x)" (-sin (x+2)) (sin(-x-2))

stSin5 :: Test
stSin5 = TestCase $ assertEqual "sin(pi-x)" (sin x) (sin(pi-x))

stSin6 :: Test
stSin6 = TestCase $ assertEqual "sin(3*pi/2)" (-1) (sin(3*(pi:: Expr)/2))

stSin7 :: Test
stSin7 = TestCase $ assertEqual "sin(9*pi-x)" (sin x) (sin(9*(pi :: Expr)-x))

stSin :: Test
stSin = TestList [stSin1, stSin2, stSin3, stSin4, stSin5, stSin6, stSin7]


---

st1Cos :: Test
st1Cos = TestCase $ assertEqual "cos(2*pi + x)" (cos x) (cos(2*pi+x))

st2Cos :: Test
st2Cos = TestCase $ assertEqual "cos(pi/2)" 0 (cos((pi :: Expr)/2))

st3Cos :: Test
st3Cos = TestCase $ assertEqual "cos(0)" 1 (cos(0 :: Expr))

st4Cos :: Test
st4Cos = TestCase $ assertEqual "sin(-x)" (cos x) (cos(-x))

st5Cos :: Test
st5Cos = TestCase $ assertEqual "cos(pi-x)" (-cos x) (cos(pi-x))

st6Cos :: Test
st6Cos = TestCase $ assertEqual "cos(3*pi/2)" 0 (cos(3*(pi :: Expr)/2))

stCos :: Test
stCos = TestList [st1Cos, st2Cos, st3Cos, st4Cos, st5Cos, st6Cos]

simplifyTrigTests :: Test
simplifyTrigTests = TestLabel "Simplififcacion de funciones trigonometricas" $ TestList [stSin, stCos]

--

assertNotEqual :: Eq a => String -> a -> a -> Assertion
assertNotEqual msg a b = assertBool msg (a /= b)

sr1 :: Test
sr1 = TestCase $ assertNotEqual "sqrt (x ** 2) != x" (sqrt (x**2)) x

sr2 :: Test
sr2 = TestCase $ assertEqual "(sqrt x) ** 2" (sqrt x ** 2) x

symplifyRadical :: Test
symplifyRadical = TestLabel "Simplififcacion de radicales" $ TestList [sr1, sr2]

main :: IO Counts
main = runTestTT $ TestList [symplifyBasic, symplifyConst, symplifyFraction, symplifyRadical, simplifyTrigTests, Algebraic.tests, Fu.tests]
