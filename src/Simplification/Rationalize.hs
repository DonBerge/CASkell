{-# LANGUAGE ViewPatterns #-}

module Simplification.Rationalize (
    rationalize,
    rationalSimplify,
    numerator,
    denominator
) where

import Expr
import Structure
import Simplification.PolyTools
import Data.Bifunctor
import Data.List

import qualified Data.Number as Number
import qualified Simplification.Algebraic as Algebraic

rationalize :: Expr -> Expr
rationalize (structure -> Pow x y) = (rationalize x) ** y
rationalize (structure -> Mul xs) = product $ fmap rationalize xs
rationalize u@(structure -> Add (f :|| _)) = let
                                                g = rationalize f
                                                h = rationalize (u - f)
                                             in
                                                rationalizeSum g h
rationalize u = u

rationalizeSum :: Expr -> Expr -> Expr
rationalizeSum u v = let
                        m = numerator u
                        r = denominator u
                        n = numerator v
                        s = denominator v
                      in
                        if r == 1 && s == 1
                            then u + v
                            else (rationalizeSum (m*s) (n*r)) / (r*s)
rationalSimplify :: Expr -> Expr
rationalSimplify u = let
                        u' = rationalize u
                        n = Algebraic.expand $ numerator u'
                        d = Algebraic.expand $ denominator u'
                        v = variables n `union` variables d
                        ggcd = polyGCD n d v
                        n' = recQuotient n ggcd v
                        d' = recQuotient d ggcd v
                    in
                        simplifyNumberAndSign n' d' v
    where
        -- Obtener los coeficientes que sean numeros
        numberCoefficientList (structure -> Number p) = [p]
        numberCoefficientList (structure -> Mul ((structure -> Number p) :|| _)) = [p]
        numberCoefficientList (structure -> Add us) = concatMap numberCoefficientList us
        numberCoefficientList _ = [1]

        -- signNormalized = normalized

        simplifyNumbers _ 0 = (undefinedExpr "Division by zero", undefinedExpr "Division by zero")
        simplifyNumbers 0 _ = (0,1)
        simplifyNumbers n d = let
                                c = numberCoefficientList n ++ numberCoefficientList d
                                -- Obtener el lcm de los denominadores y el gcd de los numeradores
                                (n',d') = foldr (\x -> bimap (gcd (Number.numerator x)) (lcm (Number.denominator x))) (0,1) c  -- n' is lcm of the denominators and d' is the gcd of the numerators
                                c' = fromInteger n' / fromInteger d'
                             in 
                                (n / c', d / c')
        
        simplifySign n d v = let
                                normn = normalized n v
                                normd = normalized d v
                                n' = if normn then n else negate n
                                d' = if normd then d else negate d
                             in
                                if normn == normd -- if both n and d had the same sign, do nothing otherwise negate the quotient
                                    then n' / d'
                                    else negate (n' / d')
        simplifyNumberAndSign n d v = let
                                        (n',d') = simplifyNumbers n d
                                      in
                                        simplifySign n' d' v