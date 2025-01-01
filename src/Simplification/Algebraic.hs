module Simplification.Algebraic (
    expand
) where

import Expr
import PExpr

import Symplify

import Math.Combinatorics.Exact.Binomial (choose)

expr :: PExpr -> Expr
expr = return 

expand :: Expr -> Expr
expand x = x >>= expand'

expand' :: PExpr -> Expr
expand' (Add []) = 0
expand' (Add (x:xs)) = expand' x + expand' (Add xs)
                        

expand' (Mul []) = 1
expand' (Mul (x:xs)) = expand' (Mul xs) >>= expandProduct x

expand' (Pow b e)
    | isInteger e && e >= 2 = do
                                b' <- expand' b
                                e' <- numerator e
                                expandPower b' e'

expand' u = expr u


expandProduct :: PExpr -> PExpr -> Expr
expandProduct (Add []) _ = 0
expandProduct (Add (r:rs)) s = expandProduct r s + expandProduct (Add rs) s
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = expr r * expr s

-- Se asume que n es no negativo
expandPower :: PExpr -> Integer -> Expr
expandPower (Add []) 0 = 1
expandPower (Add []) _ = 0
expandPower (Add (f:rs)) n = sum [ do
                                    cf' <- cf k
                                    cr' <- expandPower (Add rs) k
                                    expandProduct cf' cr'  | k <- [0..n] ]
    where
        coeff:: Integer -> Expr
        coeff k = fromInteger (choose n k)
        
        expo :: Integer -> Expr
        expo k = fromInteger (n-k)

        cf :: Integer -> Expr
        cf k = coeff k * expr f ** expo k
expandPower u n = expr $ Pow u $ fromInteger n