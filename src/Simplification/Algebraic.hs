{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Simplification.Algebraic (
    expand,
    expand'
) where


import Expr
import Structure

import Simplification.Rationalize
import Math.Combinatorics.Exact.Binomial (choose)

expand' = expand . return

{-|
    Expansión de expresiones, una expresion esta expandida si `variables` no contiene ninguna suma
-}
expand :: Expr -> Expr
expand (structure -> Add xs) = expandFraction $ sum $ fmap expand xs
expand (structure -> Mul xs) = foldr1 expandProduct $ fmap expand xs
expand (structure -> Pow b e)
    | Number f <- structure e = let
                                    b' = expand b
                                    (fl, m) = properFraction f
                                 in 
                                    expandProduct (expandPower b' fl) (b' ** (number m))
    | otherwise = let
                    b' = expand b
                    e' = expand e
                  in
                    expandFraction $ b' ** e'
expand (structure -> Fun f xs) = construct $ Fun f $ fmap expand xs
expand u = u

{-|
    Expansion utilizando la propiedad distributiva
    \[
        a(\sum b_i) = \sum a\cdot b_i
    \]
-}
expandProduct :: Expr -> Expr -> Expr
expandProduct (structure -> Add rs) s = expand (sum $ fmap (`expandProduct` s) rs) -- auto simplificacion puede generar terminos no expandidos, hay que expandir de nuevo
expandProduct r s@(structure -> Add _) = expandProduct s r
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
expandPower (structure -> Add (f :|| rs)) n = let
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