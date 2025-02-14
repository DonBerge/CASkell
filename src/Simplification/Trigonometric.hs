{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Simplification.Trigonometric where

import Expr
import Structure
import Classes.Assumptions
import Data.Bifunctor (Bifunctor(first, second, bimap))
import Simplification.Algebraic (expandMainOp)
import Math.Combinatorics.Exact.Binomial (choose)

{-|
    Reemplaza las ocurrencias de tan, cot, sec y csc por sus equivalentes en seno y coseno.
-}
trigSubstitute :: Expr -> Expr
trigSubstitute = mapStructure trigSubstitute . trigSubstitute'
    where
        trigSubstitute' (structure -> Tan x) = sin x / cos x
        trigSubstitute' (structure -> Cot x) = cos x / sin x
        trigSubstitute' (structure -> Sec x) = 1 / cos x
        trigSubstitute' (structure -> Csc x) = 1 / sin x
        trigSubstitute' x = x

{-|
    Expansion de las funciones trigonometricas.
-}
trigExpand :: Expr -> Expr
trigExpand = mapStructure trigExpand . trigExpand'
    where
        trigExpand' (structure -> Sin x) = fst $ expandTrigRules x
        trigExpand' (structure -> Cos x) = snd $ expandTrigRules x
        trigExpand' x = x

        expandTrigRules u@(structure -> Add (x :|| _)) = 
            let
                f = expandTrigRules x
                r = expandTrigRules (u - x)
                s = (fst f) * (snd r) + (snd f) * (fst r)
                c = (snd f) * (snd r) - (fst f) * (fst r)
            in
                (s,c)
        expandTrigRules u@(structure -> Mul (f :|| _)) = case structure f of
                                                            Number f' | true (isInteger f') -> let
                                                                                                    f'' = toInteger f'
                                                                                                    u' = u / f
                                                                                                in
                                                                                                    (multipleAngleSin f'' u', multipleAngleCos f'' u')
                                                            _ -> (sin u, cos u)
        expandTrigRules u = (sin u, cos u)


        multipleAngleSin n (structure -> Add us) = trigExpand $ sin $ sum $ fmap ((fromInteger n)*) us
        multipleAngleSin n x
            | n < 0 = - (multipleAngleSin (-n) x)
        multipleAngleSin 0 _ = 0
        multipleAngleSin 1 x = sin x
        multipleAngleSin n x = 2 * cos x * (multipleAngleSin (n-1) x) - multipleAngleSin (n-2) x
        
        multipleAngleCos n (structure -> Add us) = trigExpand $ cos $ sum $ fmap ((fromInteger n)*) us
        multipleAngleCos n x
            | n < 0 = multipleAngleCos (-n) x
        multipleAngleCos 0 _ = 1
        multipleAngleCos 1 x = cos x
        multipleAngleCos n x = 2 * cos x * (multipleAngleCos (n-1) x) - multipleAngleCos (n-2) x

{-|
    Contraccion de las funciones trigonometricas.
-}
contractTrig :: Expr -> Expr
contractTrig = mapStructure contractTrig . contractTrig'
    where
        contractTrig' v@(structure -> Pow _ _) = contractTrigRules $ expandMainOp v
        contractTrig' v@(structure -> Mul _) = contractTrigRules $ expandMainOp v
        contractTrig' v = v

        contractTrigRules v@(structure -> Pow _ _) = contractTrigPower v
        contractTrigRules v@(structure -> Mul _) =
            let
                (c,d) = separateSinCos v
            in
                if d == 1
                    then v
                else case structure d of
                        Sin _ -> v
                        Cos _ -> v
                        Pow _ _ -> expandMainOp (c * contractTrigPower d)
                        Mul _ -> expandMainOp (c * contractTrigProduct d)
                        _ -> fail "Contract trig rules: Unreachable case"
        contractTrigRules (structure -> Add us) = sum $ fmap trigRules us
            where
                trigRules v@(structure -> Pow _ _) = contractTrigRules v
                trigRules v@(structure -> Mul _) = contractTrigRules v
                trigRules v = v
        contractTrigRules v = v

        
        isSinOrCos (structure -> Sin _) = True
        isSinOrCos (structure -> Cos _) = True
        isSinOrCos (structure -> Pow v w)
            | true $ isSinOrCos v &&& isInteger w = True
        isSinOrCos _ = False
        
        separateSinCos (structure -> Mul (u:|| v:| us)) = bimap product product $ separateSinCos' (u:v:us)
            where
                separateSinCos' [] = ([],[])
                separateSinCos' (u:us)
                    | isSinOrCos u = second (u:) $ separateSinCos' us
                    | otherwise = first (u:) $ separateSinCos' us
        separateSinCos u
            | isSinOrCos u = (1,u)
            | otherwise = (u,1)
        

        contractTrigProduct = undefined

{-|
    Contraccion de potencias de senos y cosenos usando las siguientes identidades
-}
contractTrigPower :: Expr -> Expr
contractTrigPower a@(structure -> Pow u (structure -> Number n))
    | true $ isInteger n &&& isPositive n = case structure u of
                                                Sin u' -> makeSin (toInteger n) u'
                                                Cos u' -> makeCos (toInteger n) u'
                                                _ -> a
    where
        exprBinom n k = fromInteger $ choose n k

        -- contraer 
        makeCos :: Integer -> Expr -> Expr
        makeCos n x
            | n < 0 = (cos x) ** (fromInteger n)
            | even n = (exprBinom n (n `div` 2)) / 2^n + 2 ^^ (1-n) * sum [ makeSumExpr j | j <- [0.. (n `div` 2 - 1)]]
            | otherwise = 2^^(1-n) * sum [makeSumExpr j | j <- [0..n `div` 2]]
            where
                makeSumExpr j = exprBinom n j * cos (fromInteger (n-2*j))*x
        
        makeSin :: Integer -> Expr -> Expr
        makeSin n x
            | n < 0 = (sin x) ** (fromInteger n)
            | even n = (-1/2)^n * (exprBinom n (n `div` 2)) / 2^n + (-1)^(n `div` 2) * (2^^(1-n))  * sum [ makeSumExpr cos j | j <- [0.. (n `div` 2 - 1)]]
            | otherwise = (-1)^((n-1) `div` 2) * 2 ^^ (1-n) * sum [makeSumExpr sin j | j <- [0..n `div` 2]]
            where
                makeSumExpr f j = (-1) ^ j * exprBinom n j * (f ((fromInteger (n-2*j))*x))

contractTrigPower a = a