module Main (
    main
) where

import Test.HUnit
import Expr
import Simplification.TrigSimp
import Test.HUnit (Test(TestList))

x = symbol "x"

tr1T1 :: Test
tr1T1 = TestCase $ assertEqual "tr0: 2*csc(x) + sec(x)" (1/(cos x) + 2 / sin(x)) ((2* csc x + sec x) >>= tr1)

tr2T1 :: Test
tr2T1 = TestCase $ assertEqual "tr1: tan(x)" (sin x / cos x) (tan x >>= tr2)

tr2T2 :: Test
tr2T2 = TestCase $ assertEqual "tr1: cot(x)" (cos x / sin x) (cot x >>= tr2)

tr2T3 :: Test
tr2T3 = TestCase $ assertEqual "tr1: tan(tan(x) - sin(x)/cos(x))" 0 ((tan(tan(x) - sin(x)/cos(x))) >>= tr2)

tr2T = TestList [tr2T1, tr2T2, tr2T3]

-----------------

tr2iT1 :: Test
tr2iT1 = TestCase $ assertEqual "tr2i: sin(x)/cos(x)" (tan x) ((sin x / cos x) >>= tr2i)

tr2iT2 :: Test
tr2iT2 = TestCase $ assertEqual "tr2i: cos(x)/sin(x)" (cot x) ((cos x / sin x) >>= tr2i)

tr2iT3 :: Test
tr2iT3 = TestCase $ assertEqual "tr2i: sin(x) ** 2 / cos(x) ** 2" (tan(x) ** 2) ((sin(x) ** 2 / cos(x) ** 2) >>= tr2i)

tr2iT = TestList [tr2iT1]

main :: IO Counts
main = runTestTT $ TestList [ tr1T1, tr2T, tr2iT]