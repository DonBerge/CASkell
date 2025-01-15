module Simplification.PolyTools where
import Prelude hiding (exponent)

import Expr
import PExpr

import qualified Number as N

import Control.Monad (unless, foldM)
import Symplify (simplifyPow, freeOf, simplifyDiv, simplifySum, simplifyProduct, simplifySub, denominator)
import qualified Simplification.Algebraic as Algebraic

import Data.List

isSymbol :: PExpr -> Bool
isSymbol (Fun _ _) = True
isSymbol _ = False

coefficientMonomialGPE :: MonadFail m => PExpr -> PExpr -> m (PExpr, Integer)
coefficientMonomialGPE u x
    | u == x = return (1, 1)
coefficientMonomialGPE (Pow base (Number exponent)) x
    | true $ (base == x) &&& isInteger exponent &&& (exponent > 1) = return (1, N.numerator exponent)
coefficientMonomialGPE u@(Mul us) x = mapM (`coefficientMonomialGPE` x) us >>= foldM combine (u,0)
    where
        combine (c,m) (_,0) = return (c,m)
        combine (_, _) (_, m) = simplifyPow x (fromInteger m) >>= simplifyDiv u >>= \c -> return (c, m)
coefficientMonomialGPE u x
    | freeOf u x = return (u,0)
    | otherwise = fail "Not a general monomial expression"

degreeGPE :: MonadFail m => PExpr -> PExpr -> m Integer
degreeGPE (Add us) x = foldM (\d u -> max d . snd <$> coefficientMonomialGPE u x) (-1) us --maximum $ map (`degreeGPE` x) us
degreeGPE u x = snd <$> coefficientMonomialGPE u x

coefficientGPE :: MonadFail m => PExpr -> PExpr -> Integer -> m PExpr
coefficientGPE u@(Add us) x j
    | u == x = if j == 1 then return 1 else return 0
    | otherwise = foldM combine 0 us
        where
            combine c mon = do
                                (c', m) <- coefficientMonomialGPE mon x
                                if m == j
                                    then simplifySum [c,c']
                                    else return c
coefficientGPE u x j = do
                        (c,m) <- coefficientMonomialGPE u x
                        if j == m
                            then return c
                            else return 0

leadingMonomial :: MonadFail m => PExpr -> [PExpr] -> m PExpr
leadingMonomial p symbols = do
                                unless (all isSymbol symbols) $ fail "leadingMonomial: not all arguments are symbols"
                                leadingMonomial' symbols p
    where
        leadingMonomial' [] u = return u
        leadingMonomial' (x:l) u = do
                                    m <- degreeGPE u x
                                    c <- coefficientGPE u x m
                                    xm <- simplifyPow x (fromInteger m)
                                    lm <- leadingMonomial' l c
                                    simplifyProduct [xm,lm]


polyDivide :: PExpr -> PExpr -> [PExpr] -> Fail (PExpr, PExpr)
polyDivide u v l = polyDivide' 0 u
    where
        vl = leadingMonomial v l
    
        g (Add us) vm = mapM (`g` vm) us >>= simplifySum
        g w wm = do
                    w' <- simplifyDiv w wm
                    dw <- denominator w'
                    if dw == 1
                        then return w'
                        else return 0

        polyDivide' q r = do
                            f <- vl >>= g r
                            if f == 0
                                then return (q,r)
                                else do
                                        q' <- simplifySum [q,f]
                                        r' <- Algebraic.expand (simplifyProduct [f,v] >>= simplifySub r)
                                        polyDivide' q' r'
    
variables :: PExpr -> [PExpr]
variables (Number _) = []
variables u@(Pow v w)
    | true (isInteger w &&& w > 1) = variables v
    | otherwise = [u]
variables (Add us) = foldl union [] $ map variables us
variables (Mul us) = foldl union [] $ map variables us
variables u = [u]

divide u v = do
                u' <- Algebraic.expand u
                v' <- Algebraic.expand v
                let symbols = variables u'
                (u'', v'') <- polyDivide u' v' symbols
                return (u'', v'')