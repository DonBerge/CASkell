{-# LANGUAGE ViewPatterns #-}

module Simplification.Rationalize (
    rationalize,
    rationalSimplify
) where

import Expr
import Structure

import Classes.Assumptions

import qualified Number as N

numerator :: Expr -> Expr
numerator (structure -> Number n) = fromInteger $ N.numerator n
numerator (structure -> Mul xs) = product $ fmap numerator xs
numerator (structure -> Pow _ y)
    | true $ isNegative y = 1
numerator (structure -> Exp x)
    | true $ isNegative x = 1
numerator x = x

denominator :: Expr -> Expr
denominator (structure -> Number n) = fromInteger $ N.denominator n
denominator (structure -> Mul xs) = product $ fmap denominator xs
denominator u@(structure -> Pow _ y)
    | true $ isNegative y = recip u
denominator (structure -> Exp x)
    | true $ isNegative x = exp (-x)
denominator _ = 1

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
rationalSimplify = undefined
    where
        numberCoefficientList (structure -> Number p) = [p]
        numberCoefficientList (structure -> Mul ((structure -> Number p) :|| _)) = [p]
        numberCoefficientList (structure -> Add us) = concatMap numberCoefficientList us
        numberCoefficientList _ = [1]


{-

rationalSimplify :: EvalSteps PExpr -> EvalSteps PExpr
rationalSimplify = (=<<) rationalSimplify'
    where
        numberCoefficientList (Number p) = [p]
        numberCoefficientList (Mul []) = [0]
        numberCoefficientList (Mul (Number p:_)) = [p]
        numberCoefficientList (Add us) = concatMap numberCoefficientList us
        numberCoefficientList _ = [1]

        signNormalized p v = do
                                lcp <- foldM leadingCoefficient p v
                                return (lcp == 1 || lcp == 0)

        simplfyNumbers _ 0 = fail "Division by zero"
        simplfyNumbers 0 _ = return (0,1)
        simplfyNumbers n d = let
                                c = numberCoefficientList n ++ numberCoefficientList d
                                (n',d') = foldr (\x -> bimap (gcd (N.numerator x)) (lcm (N.denominator x))) (0,1) c  -- n' is lcm of the denominators and d' is the gcd of the numerators
                             in do
                                  c' <- simplifyDiv (fromInteger n') (fromInteger d')
                                  n'' <- simplifyDiv n c'
                                  d'' <- simplifyDiv d c'
                                  return (n'',d'')

        simplifySign n d v = do
                                normn <- signNormalized n v
                                normd <- signNormalized d v
                                n' <- if normn then return n else simplifyNegate n
                                d' <- if normd then return d else simplifyNegate d
                                if normn == normd -- if both n and d had the same sign, do nothing otherwise negate the quotient
                                    then simplifyDiv n' d'
                                    else simplifyDiv n' d' >>= simplifyNegate
        
        simplifyNumberAndSign n d v = do
                                        (n',d') <- simplfyNumbers n d
                                        simplifySign n' d' v
        rationalSimplify' u = do
                                u' <- rationalize u
                                n <- Algebraic.expand $ numerator u'
                                d <- Algebraic.expand $ denominator u'
                                let v = variables n `union` variables d
                                ggcd <- polyGCD n d v
                                n' <- recQuotient n ggcd v
                                d' <- recQuotient d ggcd v
                                simplifyNumberAndSign n' d' v
-}