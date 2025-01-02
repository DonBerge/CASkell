module Simplification.Algebraic (
    expand
) where

import Expr
import PExpr

import Symplify

import Math.Combinatorics.Exact.Binomial (choose)

expr :: PExpr -> Expr
expr = return 

numerator :: PExpr -> Expr
numerator (Add []) = 0
numerator (Mul []) = 1
numerator (Mul xs) = product $ map numerator xs
numerator (Pow _ y)
    | isNegative y = 1
numerator (Exp x)
    | isNegative x = 1
numerator x = expr x    

denominator :: PExpr -> Expr
denominator (Add []) = 1
denominator (Mul []) = 1
denominator (Mul xs) = product $ map denominator xs
denominator (Pow x y)
    | isNegative y = expr x ** (negate $ expr y)
denominator (Exp x)
    | isNegative x = exp $ negate $ expr x
denominator x = 1

expand :: Expr -> Expr
expand x = x >>= expand'

expand' :: PExpr -> Expr
expand' (Add []) = 0
expand' (Add [x]) = expand' x
expand' (Add (x:xs)) = expand' x + expand' (Add xs)
                        

expand' (Mul []) = 1
expand' (Mul [x]) = expand' x
expand' (Mul (x:xs)) = do
                        x' <- expand' x
                        xs' <- expand' (Mul xs)
                        expandProduct x' xs'

expand' (Pow b e)
    | isInteger e && e >= 2 = do
                                b' <- expand' b
                                expandPower b' (numberNumerator e)
    | otherwise = do
                    b' <- expand' b
                    e' <- expand' e
                    return $ Pow b' e'

expand' (Fun f xs) = Fun f <$> mapM expand' xs

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