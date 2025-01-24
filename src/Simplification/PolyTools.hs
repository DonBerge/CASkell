{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Simplification.PolyTools where
import Prelude hiding (exponent)

import PExpr

import Data.Bifunctor

import qualified Number as N

import Control.Monad (unless, foldM)
import Symplify
import qualified Simplification.Algebraic as Algebraic

import Data.List
import Simplification.Rationalize (rationalize)
import Classes.EvalSteps (addStep)
import Data.List (unwords)


isSymbol :: PExpr -> Bool
isSymbol (Fun _ _) = True
isSymbol _ = False

-- The list [c, m] where m is the degree of the monomial and c is the coefficient of xm
coefficientMonomialGPE :: PExpr -> PExpr -> EvalSteps (PExpr, Integer)
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
    | otherwise = fail $ show u ++ " is not a general monomial expression over " ++ show x

degreeGPE :: PExpr -> PExpr -> EvalSteps Integer
degreeGPE (Add us) x = foldM (\d u -> max d . snd <$> coefficientMonomialGPE u x) (-1) us --maximum $ map (`degreeGPE` x) us
degreeGPE u x = snd <$> coefficientMonomialGPE u x

coefficientGPE :: PExpr -> PExpr -> Integer -> EvalSteps PExpr
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

coefficientListGPE :: PExpr -> PExpr -> EvalSteps [PExpr]
coefficientListGPE u x = do
                            m <- degreeGPE u x
                            mapM (coefficientGPE u x) [0..m]

leadingCoefficient :: PExpr -> PExpr -> EvalSteps PExpr
leadingCoefficient u x = do
                            m <- degreeGPE u x
                            coefficientGPE u x m

leadingMonomial :: PExpr -> [PExpr] -> EvalSteps PExpr
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

-- division with integer coefficients
-- u and v are rational multivariate polynomials
-- l is a list of symbols
recPolyDivide :: PExpr -> PExpr -> [PExpr] -> EvalSteps (PExpr, PExpr)
recPolyDivide u v [] = do
                        udivv <- simplifyDiv u v
                        if true (isInteger udivv)
                            then return (udivv, 0)
                            else return (0,u)
recPolyDivide u v (x:tl) = do 
                                m <- degreeGPE u x
                                n <- degreeGPE v x
                                lcv <- leadingCoefficient v x
                                recPolyDivideLoop lcv 0 u m n
    where
        recPolyDivideLoop lcv q r m n
            | m >= n = do
                        lcr <- leadingCoefficient r x
                        d <- recPolyDivide lcr lcv tl
                        if snd d /= 0
                            then Algebraic.expand (return q) >>= \q' -> return (q',r)
                            else do
                                let c = fst d
                                xmn <- simplifyPow x (fromInteger $ m-n)
                                q' <- simplifyProduct [c,xmn] >>= \t -> simplifySum [q,t] -- q' = q + c*x^(m-n)
                                r' <- Algebraic.expand $ simplifyProduct [c,v, xmn] >>= simplifySub r -- r' = r - c*v*x^(m-n)
                                m' <- degreeGPE r x
                                recPolyDivideLoop lcv q' r' m' n
            | otherwise = Algebraic.expand (return q) >>= \q' -> return (q',r)


recQuotient :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
recQuotient u v l = fst <$> recPolyDivide u v l

recRemainder :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
recRemainder u v l = snd <$> recPolyDivide u v l

-- Division de polinomios basada en monomios
polyDivide :: PExpr -> PExpr -> [PExpr] -> EvalSteps (PExpr, PExpr)
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

divmod :: EvalSteps PExpr -> EvalSteps PExpr -> EvalSteps (PExpr, PExpr)
divmod p q = do
                p' <- Algebraic.expand p
                q' <- Algebraic.expand q
                let vars = variables q'
                polyDivide p' q' vars

quotient :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
quotient p q l = fst <$> polyDivide p q l

remainder :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
remainder p q l = snd <$> polyDivide p q l


---

pseudoDivision :: PExpr -> PExpr -> PExpr -> EvalSteps (PExpr, PExpr)
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
            | m == 0 && n == 0 = polyDivide u v [x] -- both u and v do not have x as main variable
            | m >= n = do
                        lcs <- coefficientGPE s x m -- 0
                        x' <- simplifyPow x (fromInteger $ m-n) -- 1
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

pseudoRem :: PExpr -> PExpr -> PExpr -> EvalSteps PExpr
pseudoRem u v x = snd <$> pseudoDivision u v x

-- Find the unit normal form of u, the coefficient domain is the rational numbers
-- A unit is an expresion that has a multiplicative inverse.

-- An expression u is unit normal if
-- 1. u = 0 or u = 1
-- 2. u = v*w, where v and w are unit normals
-- 3. There exists a unique unit c such that c*u is unit normal
-- In Z, the unit normal expresions are the positive integers, in Q,
-- the only unique normal elements are 0 and 1

-- A polynomial u in K[x] is unit normal if lc(u) is unit normal
-- A polynomial u in K[x,y,...] is unit normal if lc(u) is unit normal in K[y,...]
normalize :: Foldable t => PExpr -> t PExpr -> EvalSteps PExpr
normalize 0 _ = return 0
normalize u l = foldM leadingCoefficient u l  >>= simplifyDiv u

-- Check if a polynomial is unit normal

normalized :: Foldable t => PExpr -> t PExpr -> EvalSteps Bool
normalized u l = do
                    lc <- foldM leadingCoefficient u l
                    return $ lc == 1 || lc == 0

polyPrimitivePart :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyPrimitivePart 0 _ _ = return 0
polyPrimitivePart u x r = do
                            contU <- polyContent u x r
                            quotient u contU (x:r)

polyGCD :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyGCD 0 v l = normalize v l
polyGCD u 0 l = normalize u l
polyGCD u v l = polyGCDRec u v l >>= (`normalize` l)
    where
        polyGCDRec _ _ [] = return 1 -- gcd(u,v) where u and v are non-zero rationals is 1
        polyGCDRec u v l@(x:rest) = do
                                    contU <- polyContent u x rest
                                    contV <- polyContent v x rest
                                    
                                    d <- polyGCDRec contU contV rest
                                    
                                    ppU <- quotient u contU l -- primitive part of u
                                    ppV <- quotient v contV l -- primitive part of v

                                    rp <- gcdRec' x rest ppU ppV

                                    Algebraic.expand $ simplifyProduct [d,rp]
        
        gcdRec' _ _ ppU 0 = return ppU
        gcdRec' x rest ppU ppV = do
                                    r <- pseudoRem ppU ppV x
                                    ppR <- polyPrimitivePart r x rest
                                    degU <- degreeGPE ppU x
                                    if ppR == ppU && degU == 0 -- Avoids infinite recursion
                                        then return 1
                                        else gcdRec' x rest ppV ppR 

remainderSequence :: PExpr -> PExpr -> [PExpr] -> EvalSteps [PExpr]
remainderSequence _ _ [] = error "Remainder sequence undefined for empty lists"
remainderSequence u v (x:rest) = do
                                    ppU <- polyPrimitivePart u x rest
                                    ppV <- polyPrimitivePart v x rest
                                    remainderSequence' ppU ppV
    where
        remainderSequence' ppU 0 = return [ppU, 0]
        remainderSequence' ppU ppV = do
                                        r <- pseudoRem ppU ppV x
                                        ppR <- polyPrimitivePart r x rest
                                        rs <- remainderSequence' ppV ppR
                                        return $ ppU:rs

gcdList :: [PExpr] -> [PExpr] -> EvalSteps PExpr
gcdList [] _ = return 0
gcdList [p] l = normalize p l 
gcdList (p:ps) r = do
                    ps' <- gcdList ps r
                    polyGCD p ps' r

polyContent :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyContent u x r = do
                      cfl <- coefficientListGPE u x 
                      gcdList cfl r

polGCD :: EvalSteps PExpr -> EvalSteps PExpr -> [EvalSteps PExpr] -> EvalSteps PExpr
polGCD u v l = do
                u' <- Algebraic.expand u
                v' <- Algebraic.expand v
                l' <- sequence l
                polyGCD u' v' l'

lcmList :: [PExpr] -> EvalSteps PExpr
lcmList us = do
                n <- Algebraic.expand $ simplifyProduct us
                let v = variables n
                -- addStep $ "Calculating the lcm of " ++ show us ++ " with respect to " ++ show v
                let d = removeEachElement us
                d' <- mapM (Algebraic.expand . simplifyProduct) d >>= (`gcdList` v)
                quotient n d' v
            where
                removeEachElement :: [a] -> [[a]]
                removeEachElement xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

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
                                n' <- quotient n ggcd v
                                d' <- quotient d ggcd v
                                simplifyNumberAndSign n' d' v