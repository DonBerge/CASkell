{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-|
    Module      : Simplification.Algebraic
    Description : Módulo que implementa la expansión de expresiones algebraicas.
-}
module Simplification.Algebraic (
    expand,
    expandMainOp
) where


import Expr
import Structure

import Math.Combinatorics.Exact.Binomial (choose)
import Assumptions (true, Assumptions (isInteger))

{-|
    Expansión de expresiones, una expresion esta expandida si ninguna suma es el operando de un producto o de una potencia
    a exponente entero.
-}
expand :: Expr -> Expr
expand (Add xs) = expandFraction $ sum $ fmap expand xs
expand (Mul xs) = foldr1 expandProduct $ fmap expand xs
expand (Pow b e)
    | Number f <- e = let
                                    b' = expand b
                                    (fl, m) = properFraction f
                                 in 
                                    expandProduct (expandPower b' fl) (b' ** (fromNumber m))
    | otherwise = let
                    b' = expand b
                    e' = expand e
                  in
                    expandFraction $ b' ** e'
expand u@(Fun _ _) = mapStructure expand u --construct $ Fun f $ fmap expand xs
expand u = u

{-|
    Expansion utilizando la propiedad distributiva
    \[
        a(\sum b_i) = \sum a\cdot b_i
    \]
-}
expandProduct :: Expr -> Expr -> Expr
expandProduct (Add rs) s = expand (sum $ fmap (`expandProduct` s) rs) -- auto simplificacion puede generar terminos no expandidos, hay que expandir de nuevo
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = expandFraction $ r * s 

{-|
    Expansion utilizando el binomio de Newton
    \[
        (a + b)^n = \sum_{k=0}^{n} \binom{n}{k} a^{n-k} b^k
    \]
-}

expandPower :: Expr -> Integer -> Expr
expandPower u n
    | n < 0 = 1 / expandPower u (-n)
expandPower _ 0 = 1
expandPower u 1 = u
expandPower (Add (f :|| rs)) n = let
                                                rs' = sum rs
                                              in
                                                sum [expandProduct (cf k) (expandPower rs' (n-k)) | k <- [0..n]]
    where
        cf k = (fromInteger $ choose n k) * f ** (fromInteger k)
expandPower u n = u ** (fromInteger n)

{-|
    Expande el denominador de una expresion
-}
expandFraction :: Expr -> Expr
expandFraction x = let
                    n = numerator x
                    d = denominator x
                   in if d == 1
                    then x
                    else n / (expand d)

{-|
    Expande la expresion solo una vez, las subexpresiones no se expanden

    Ejemplos:
        > expandMainOp (x*(2+(1+x)²)) = x*2 + x*(1+x)²
        > expandMainOP ((x+(1+x)²)²) = x² + 2*x*(1+x)² + (1+x)⁴ 
-}
expandMainOp :: Expr -> Expr
expandMainOp (Mul xs) = foldr1 expandProduct' xs
    where
        expandProduct' u (Add us) = sum $ fmap (u*) us
        expandProduct' u@(Add _) v = expandProduct v u
        expandProduct' u v = u * v
expandMainOp (Pow b e)
    | Number f <- e, true (isInteger f) = expandPower b (toInteger f)
expandMainOp u = u