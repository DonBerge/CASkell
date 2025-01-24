module Test.Fu (
    tests
) where

import Simplification.Fu
import Expr
import Test.HUnit ( assertEqual, Test(..) )

{-# ANN module "HLint: ignore Use tan" #-}

x :: Expr
x = symbol "x"

y :: Expr
y = symbol "y"

z :: Expr
z = symbol "z"

tr1t1 :: Test
tr1t1 = TestCase $ assertEqual "tr1: sec x" (1 / cos x) (sec x >>= tr1)

tr1t2 :: Test
tr1t2 = TestCase $ assertEqual "tr1: csc x" (1 / sin x) (csc x >>= tr1)

tr1t3 :: Test
tr1t3 = TestCase $ assertEqual "tr1: cot x" (1/cos x + 2/sin x) (2*csc x + sec x  >>= tr1)

tr1tests :: Test
tr1tests = TestLabel "tr1" $ TestList [tr1t1, tr1t2, tr1t3]

----

tr2t1 :: Test
tr2t1 = TestCase $ assertEqual "tr2: tan x" (sin x / cos x) (tan x >>= tr2)

tr2t2 :: Test
tr2t2 = TestCase $ assertEqual "tr2: cot x" (cos x / sin x) (cot x >>= tr2)

tr2t3 :: Test
tr2t3 = TestCase $ assertEqual "tr2: tan(tan x - sin x / cos x))" 0 (tan(tan x - sin x /cos x) >>= tr2)

tr2tests :: Test
tr2tests = TestLabel "tr2" $ TestList [tr2t1, tr2t2, tr2t3]

----

tr5t1 :: Test
tr5t1 = TestCase $ assertEqual "tr5: sin(x)^2" (1 - cos x **2) ( sin x **2 >>= tr5)

tr5t2 :: Test
tr5t2 = TestCase $ assertEqual "tr5: sin(x)^(-2)" (sin x ** (-2)) ( sin x **(-2) >>= tr5)

tr5t3 :: Test
tr5t3 = TestCase $ assertEqual "tr5: sin x **4 " ((1 - cos x **2)**2) ( sin x **4 >>= tr5)

tr5tests :: Test
tr5tests = TestLabel "tr5" $ TestList [tr5t1, tr5t2, tr5t3]

----

tr6t1 :: Test
tr6t1 = TestCase $ assertEqual "tr5: cos x **2" (1 - sin x **2) ( cos x **2 >>= tr6)

tr6t2 :: Test
tr6t2 = TestCase $ assertEqual "tr5: cos x **(-2)" (cos x ** (-2)) ( cos x **(-2) >>= tr6)

tr6t3 :: Test
tr6t3 = TestCase $ assertEqual "tr5: cos x **4" ((1 - sin x **2)**2) ( cos x **4 >>= tr6)

tr6tests :: Test
tr6tests = TestLabel "tr5" $ TestList [tr6t1, tr6t2, tr6t3]

----

tr7t1 :: Test
tr7t1 = TestCase $ assertEqual "tr7: cos x **2" ((1 + cos (2*x))/2) ( cos x **2 >>= tr7)

tr7t2 :: Test
tr7t2 = TestCase $ assertEqual "tr7: cos x **2 + 1" (cos (2*x)/2 + 3/2) ( cos x **2 + 1 >>= tr7)


tr7tests :: Test
tr7tests = TestLabel "tr7" $ TestList [tr7t1, tr7t2]

----

tr8t1 :: Test
tr8t1 = TestCase $ assertEqual "tr8: cos 2 * cos 3" ((cos 5 + cos 1) / 2) ( cos 2 * cos 3 >>= tr8)

tr8t2 :: Test
tr8t2 = TestCase $ assertEqual "tr8: cos 2 * sin 3" ((sin 5 + sin 1) / 2) ( cos 2 * sin 3 >>= tr8)

tr8t3 :: Test
tr8t3 = TestCase $ assertEqual "tr8: sin 2 * sin 3" ((-cos 5 + cos 1) / 2) ( sin 2 * sin 3 >>= tr8)

tr8tests :: Test
tr8tests = TestLabel "tr8" $ TestList [tr8t1, tr8t2, tr8t3]

----

tr9t1 :: Test
tr9t1 = TestCase $ assertEqual "tr9: cos 1 + cos 2" (2 * cos(1/2) * cos(3/2)) ( cos 1 + cos 2 >>= tr9)

tr9tests :: Test
tr9tests = TestLabel "tr9" $ TestList [tr9t1]

----

tr10t1 :: Test
tr10t1 = TestCase $ assertEqual "tr10: cos(x+y)" (cos x * cos y - sin x * sin y ) (cos(x+y) >>= tr10)

tr10t2 :: Test
tr10t2 = TestCase $ assertEqual "tr10: sin(x+y)" (sin x * cos y + sin y * cos x ) (sin(x+y) >>= tr10)

tr10t3 :: Test
tr10t3 = TestCase $ assertEqual "tr10: sin(x + y + z)" ((-sin z * sin y + cos z * cos y) * sin x + (sin z * cos y + sin y * cos z)*cos x) (sin(x+y+z) >>= tr10)

tr10tests :: Test
tr10tests = TestLabel "tr10" $ TestList [tr10t1, tr10t2, tr10t3]

----

tr10it1 :: Test
tr10it1 = TestCase $ assertEqual "tro10i: sin(x+y)" (cos(x+y)) ((cos x * cos y - sin x * sin y) >>= tr10i)

tr10it2 :: Test
tr10it2 = TestCase $ assertEqual "tro10i: cos(x+y)" (cos 3 + sin 4) ((cos 1 * sin 3 + sin 1 * cos 3 + cos 3) >>= tr10i)

tr10itests :: Test
tr10itests = TestLabel "tr10i" $ TestList [tr10it1, tr10it2]

----

tr11t1 :: Test
tr11t1 = TestCase $ assertEqual "tr11: sin(2*x)" (2 * sin x * cos x ) (sin(2*x) >>= tr11)

tr11t2 :: Test
tr11t2 = TestCase $ assertEqual "tr11: cos(2*x)" (cos x ** 2 - sin x ** 2) (cos(2*x) >>= tr11)

tr11t3 :: Test
tr11t3 = TestCase $ assertEqual "tr11: sin(4*x)" (4 * sin x * cos x *(-sin x **2 + cos x **2)) (sin(4*x) >>= tr11)

tr11tests :: Test
tr11tests = TestLabel "tr11" $ TestList [tr11t1, tr11t2, tr11t3]

----

tr12t1 :: Test
tr12t1 = TestCase $ assertEqual "tr12: tan(x+y)" ((tan x + tan y) / (1 - tan x * tan y) ) (tan(x+y) >>= tr12)

tr12tests :: Test
tr12tests = TestLabel "tr12" $ TestList [tr12t1]

----

tr13t1 :: Test
tr13t1 = TestCase $ assertEqual "tr13: tan(3)*tan(2)" (1 - (tan 2 + tan 3) / tan 5) (tan 3 * tan 2 >>= tr13)

tr13t2 :: Test
tr13t2 = TestCase $ assertEqual "tr13: cot(3)*cot(2)" (1 + (cot 2 + cot 3) * cot 5) (cot 3 * cot 2 >>= tr13)

tr13tests :: Test
tr13tests = TestLabel "tr13" $ TestList [tr13t1, tr13t2]

---

fu1 :: Test
fu1 = TestCase $ assertEqual "fu: cos^2 x + sin^2 x" 1 (fu $ cos x ** 2 + sin x ** 2)

fu2 :: Test
fu2 = TestCase $ assertEqual "fu: sin(50)**2 + cos(50)**2 + sin(pi/6)" (3/2) (fu $ sin 50 **2 + cos 50 **2 + sin((pi:: Expr)/6))

fu3 :: Test
fu3 = TestCase $ assertEqual "fu: 1- sin x ** 2" (sin x ** 2) (fu $ sin x * tan x / sec x)

fu4 :: Test
fu4 = TestCase $ assertEqual "fu: 1- sin x ** 2" (tan (4*x)) (fu $ (sin(x) + sin(3*x) + sin(5*x) + sin(7*x)) / (cos(x) + cos(3*x) + cos(5*x) + cos(7*x) ))

fu5 :: Test
fu5 = TestCase $ assertEqual "fu: 1- sin x ** 2" 1 (fu $ (sin((x+1)/(x+2))**2 + cos((x+1)/(x+2))**2))

futests :: Test
futests = TestLabel "Fu" $ TestList [fu1, fu2, fu3, fu5]


tests :: Test
tests = TestLabel "Fu" $ TestList [tr1tests, tr2tests, tr5tests, tr6tests, tr7tests, tr8tests, tr9tests, tr10tests, tr10itests, tr11tests, tr12tests, tr13tests, futests]