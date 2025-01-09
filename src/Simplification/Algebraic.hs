module Simplification.Algebraic (
    expand
) where

import Expr
import PExpr

import Symplify

import Math.Combinatorics.Exact.Binomial (choose)

expand :: Expr -> Expr
expand x = x >>= expand'

expand' :: PExpr -> Expr
expand' (Add []) = 0
expand' (Add [x]) = expand' x >>= expandFraction
expand' (Add (x:xs)) = expand' x + expand' (Add xs) >>= expandFraction
                        

expand' (Mul []) = 1
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

expandFraction :: PExpr -> Expr
expandFraction x = do
                    d <- denominator x
                    if d == 1
                        then return x
                        else numerator x / expand' d

expandProduct :: PExpr -> PExpr -> Expr
expandProduct (Add []) _ = 0
expandProduct (Add (r:rs)) s = expandProduct r s + expandProduct (Add rs) s >>= expand' -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = simplifyProduct [r,s] >>= expandFraction

-- Se asume que n es no negativo
expandPower :: PExpr -> Integer -> Expr
expandPower _ 0 = 1
expandPower u 1 = return u
expandPower 0 _ = 0
expandPower (Add []) _ = 0
expandPower (Add [f]) n = fromInteger n >>= simplifyPow f >>= expand'
expandPower (Add (f:rs)) n = sequence [ do
                                            cf' <- cf k
                                            cr' <- expandPower (Add rs) (n-k)
                                            expandProduct cf' cr'  | k <- [0..n] ] >>= simplifySum >>= expandFraction -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
    where
        cf :: Integer -> Expr
        cf k = do
                fk <- simplifyPow f (fromInteger k)
                simplifyProduct [fromInteger (choose n k), fk]
expandPower u n = fromInteger n >>= simplifyPow u >>= expandFraction