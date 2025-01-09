module Test.Fu (
    tests
) where

import Simplification.Fu
import Expr
import Test.HUnit ( assertEqual, Test(..) )

{-# ANN module "HLint: ignore Use tan" #-}

x :: Expr
x = symbol "x"

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
tr2t3 = TestCase $ assertEqual "tr2: sec x" 0 (tan(tan x - sin x /cos x) >>= tr2)

tr2tests :: Test
tr2tests = TestLabel "tr2" $ TestList [tr2t1, tr2t2, tr2t3]

tests :: Test
tests = TestLabel "Fu" $ TestList [tr1tests, tr2tests]