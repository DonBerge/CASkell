module Simplification.PolyTools where
import Prelude hiding (exponent)

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


-- Division de polinomios basada en monomios
polyDivide :: MonadFail m => PExpr -> PExpr -> [PExpr] -> m (PExpr, PExpr)
polyDivide u v l = do
                    let q = 0
                        r = u
                    vl <- leadingMonomial v l
                    f <- g r vl
                    polyDivide' q r f vl
    where
        polyDivide' q r f vl
            | f /= 0 = do
                        q <- simplifySum [q,f]
                        r <- Algebraic.expand $ simplifySub r =<< simplifyProduct [f,v]
                        f <- g r vl
                        polyDivide' q r f vl
            | otherwise = return (q,r)

        g (Add us) vm = mapM (`g` vm) us >>= simplifySum  -- g = sum ui / vm, donde ui es divisible por vm
        g w wm = do
                    w' <- simplifyDiv w wm
                    dw <- denominator w'
                    if dw == 1
                        then return w'
                        else return 0
    
variables :: PExpr -> [PExpr]
variables (Number _) = []
variables u@(Pow v w)
    | true (isInteger w &&& w > 1) = variables v
    | otherwise = [u]
variables (Add us) = foldl union [] $ map variables us
variables (Mul us) = foldl union [] $ map variables us
variables u = [u]

divmod :: MonadFail m => m PExpr -> m PExpr -> m (PExpr, PExpr)
divmod p q = do
                p' <- Algebraic.expand p
                q' <- Algebraic.expand q
                let vars = variables q'
                polyDivide p' q' vars

quotient :: MonadFail m => m PExpr -> m PExpr -> m PExpr
quotient p q = fst <$> divmod p q

remainder :: MonadFail m => m PExpr -> m PExpr -> m PExpr
remainder p q = snd <$> divmod p q


---

pseudoDivision :: MonadFail m => PExpr -> PExpr -> PExpr -> m (PExpr, PExpr)
pseudoDivision u v x = do
                        let p = 0
                        let s = u
                        m <- degreeGPE s x
                        n <- degreeGPE v x
                        let delta = max (m-n+1) 0
                        lcv <- coefficientGPE v x n
                        let sigma = 0
                        pseudoDivision' p s m n delta lcv sigma
    where
        pseudoDivision' p s m n delta lcv sigma
            | m >= n = do
                        lcs <- coefficientGPE s x m
                        x' <- simplifyPow x (fromInteger $ m-n)
                        p <- do
                                a' <- simplifyProduct [lcv,p]
                                b' <- simplifyProduct [lcs,x']
                                simplifySum [a',b']
                        s <- do
                                a' <- simplifyProduct [lcv, s]
                                b' <- simplifyProduct [lcs, v, x']
                                Algebraic.expand $ simplifySub a' b'
                        -- let sigma = sigma + 1
                        m <- degreeGPE s x
                        pseudoDivision' p s m n delta lcv (sigma+1)
            | otherwise = do
                            lcv' <- simplifyPow lcv (fromInteger $ delta-sigma)
                            q <- Algebraic.expand $ simplifyProduct [lcv',p]
                            r <- Algebraic.expand $ simplifyProduct [lcv',s]
                            return (q,r)

pseudoDivide :: MonadFail m => m PExpr -> m PExpr -> m PExpr -> m (PExpr, PExpr)
pseudoDivide u v x = do
                        u' <- Algebraic.expand u
                        v' <- Algebraic.expand v
                        x' <- x
                        pseudoDivision u' v' x'