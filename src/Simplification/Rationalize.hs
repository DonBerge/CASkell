module Simplification.Rationalize (
    rationalize,
    rationalSimplify
) where
import Symplify

import Control.Monad
import Data.Bifunctor

import Data.List

import qualified Number as N

import qualified Simplification.Algebraic as Algebraic

import Simplification.PolyTools

rationalize :: PExpr -> EvalSteps PExpr
rationalize (Pow x y) = rationalize x >>= (`simplifyPow` y)
rationalize (Mul xs) = mapM rationalize xs >>= simplifyProduct
rationalize (Add []) = return 0
rationalize (Add (x:xs)) = do
                            g <- rationalize x
                            h <- rationalize (Add xs)
                            rationalizeSum g h
rationalize u = return u
rationalizeSum :: PExpr -> PExpr -> EvalSteps PExpr

rationalizeSum u v = do
                        m <- numerator u
                        r <- denominator u
                        n <- numerator v
                        s <- denominator v
                        if r == 1 && s == 1
                            then simplifySum [u,v]
                            else do
                                    ms <- simplifyProduct [m,s]
                                    nr <- simplifyProduct [n,r]
                                    n' <- rationalizeSum ms nr
                                    d' <- simplifyProduct [r,s]
                                    simplifyDiv n' d'
                                    -- (rationalizeSum ms nr) / (simplifyProduct [r,s])

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