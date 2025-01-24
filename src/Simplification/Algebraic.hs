module Simplification.Algebraic (
    expand,
    expand'
) where

import PExpr

import Symplify

import Math.Combinatorics.Exact.Binomial (choose)

expand :: EvalSteps PExpr -> EvalSteps PExpr
expand x = x >>= expand'

expand' :: PExpr -> EvalSteps PExpr
expand' (Add []) = return 0
expand' (Add [x]) = expand' x >>= expandFraction
expand' (Add (x:xs)) = do
                        x' <- expand' x
                        xs' <- expand' (Add xs)
                        simplifySum [x',xs'] >>= expandFraction -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez

expand' (Mul []) = return 1
expand' (Mul [x]) = expand' x >>= expandFraction
expand' (Mul (x:xs)) = do
                        x' <- expand' x
                        xs' <- expand' (Mul xs)
                        expandProduct x' xs'

expand' (Pow b (Number f))
    | f > 0 = let
                (fl, m) = properFraction f
              in do 
                    b' <- expand' b
                    t' <- if f == m 
                        then return $ Pow b (Number m) 
                        else expand $ simplifyPow b' (Number m)
                    expandPower b' fl >>= expandProduct t'

expand' (Pow b e) = do
                        b' <- expand' b
                        e' <- expand' e
                        expandFraction $ Pow b' e'

expand' (Fun f xs) = (Fun f <$> mapM expand' xs) >>= expandFraction

expand' u = expandFraction u

expandFraction :: PExpr -> EvalSteps PExpr
expandFraction x = do
                    n <- numerator x
                    d <- denominator x
                    if d == 1
                        then return x
                        else expand' d >>= simplifyDiv n  --numerator x / expand' d

expandProduct :: PExpr -> PExpr -> EvalSteps PExpr
expandProduct (Add []) _ = return 0
expandProduct (Add (r:rs)) s = do
                                r' <- expandProduct r s
                                rs' <- expandProduct (Add rs) s
                                simplifySum [r',rs'] >>= expand' -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = simplifyProduct [r,s] >>= expandFraction

-- Se asume que n es no negativo
expandPower :: PExpr -> Integer -> EvalSteps PExpr
expandPower _ 0 = return 1
expandPower u 1 = return u
expandPower 0 _ = return 0
expandPower (Add []) _ = return 0
expandPower (Add [f]) n = simplifyPow f (fromInteger n) >>= expand'
expandPower (Add (f:rs)) n = sequence [ do
                                            cf' <- cf k
                                            cr' <- expandPower (Add rs) (n-k)
                                            expandProduct cf' cr'  | k <- [0..n] ] >>= simplifySum >>= expandFraction -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
    where
        cf k = do
                fk <- simplifyPow f (fromInteger k)
                simplifyProduct [fromInteger (choose n k), fk]
expandPower u n = simplifyPow u (fromInteger n) >>= expandFraction