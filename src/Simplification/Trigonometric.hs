{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Simplification.Trigonometric where

import Expr
import Structure

import qualified Data.Matrix as Matrix

import Classes.Assumptions
import Data.Bifunctor (Bifunctor(first, second, bimap))
import Simplification.Algebraic (expandMainOp)
import Math.Combinatorics.Exact.Binomial (choose)
import qualified Simplification.Algebraic as Algebraic
import Calculus.Integrate (substitute)

{-|
    Reemplaza las ocurrencias de 'tan', 'cot', 'sec' y 'csc' por sus equivalentes en seno y coseno.

    > trigSubstitute (tan x) = sin x / cos xç
    > trigSubstitute (cot x) = cos x / sin x
    > trigSubstitute (cosec x + sec y) = 1 / sin x + 1 / cos y
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
    Convierte expresiones en su forma trigonometrica expandida.

    Una expresion esta en forma trigonometrica expandida si cada argumento de un seno o coseno cumple que:

        1. No es una suma;

        2. No es un producto con un operando que es un entero.

-}
trigExpand :: Expr -> Expr
trigExpand = trigExpand' . mapStructure trigExpand
    where
        trigExpand' (structure -> Sin x) = fst $ expandTrigRules x
        trigExpand' (structure -> Cos x) = snd $ expandTrigRules x
        trigExpand' x = x

        expandTrigRules u@(structure -> Add (x :|| _)) = 
            let
                f = expandTrigRules x
                r = expandTrigRules (u - x)
                s = fst f * snd r + snd f * fst r
                c = snd f * snd r - fst f * fst r
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


        -- Calcular cos(n*x)
        multipleAngleCos n u@(structure -> Add _) = trigExpand $ cos $ Algebraic.expandMainOp $ fromInteger n * u
        multipleAngleCos 0 _ = 1
        multipleAngleCos 1 x = cos x
        multipleAngleCos n x 
            | n < 0 = multipleAngleCos (-n) x
            | otherwise = let
                            x' = symbol "_"
                            f = cheby1 x' n
                          in
                           substitute f x' (cos x)

        multipleAngleSin n u@(structure -> Add _) = trigExpand $ sin $ Algebraic.expandMainOp $ fromInteger n * u
        multipleAngleSin 0 _ = 0
        multipleAngleSin 1 x = sin x
        multipleAngleSin n x
            | n < 0 = - (multipleAngleSin (-n) x)
            | otherwise = let
                            x' = symbol "_"
                            f = cheby2 x' (n-1)
                          in
                            Algebraic.expand $ (substitute f x' (cos x)) * sin x 


{-|
    Generación de polinomios de Chebyshev de primera clase.

    Los polinomios de Chebyshev de primera clase cumplen la siguiente propiedad:
        \[T_n(\cos x) = \cos(n \cdot x)\]
    
    Por lo que son utiles para realizar la expansión de \(\cos(n \cdot x)\)
-}
cheby1 :: Integral b => Expr -> b -> Expr
cheby1 _ 0 = 1
cheby1 x 1 = x
cheby1 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2*x,-1],[1,0]] ^ (n-1)) * Matrix.fromLists [[x], [1]]

{-|
    Generación de polinomios de Chebyshev de segunda clase.

    Los polinomios de Chebyshev de segunda clase cumplen la siguiente propiedad:
        \[U_n(\cos x)\sin x = {\sin((n+1)x)}\]

    Por lo que son utiles para realizar la expansión de \(\sin(n \cdot x)\)
-}
cheby2 :: Integral b => Expr -> b -> Expr
cheby2 _ 0 = 1
cheby2 x 1 = 2*x
cheby2 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2*x,-1],[1,0]] ^ n) * Matrix.fromLists [[1], [0]]


{-|
    Conversión de expresiones trigonometricas a su forma trigonometrica contraida.

    Una expresion esta en forma trigonometrica contraida si satisface que:

        1. Cualquier producto en la expresión tiene como mucho un operando que es un seno o coseno;
        2. Una potencia con exponente entero positivo no tiene como base a un seno o coseno;
        3. Cualquier subexpresión esta en forma expandida.

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