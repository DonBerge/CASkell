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
expand' (Add xs) = sum $ map expand' xs
expand' (Mul []) = 1
expand' (Mul [x]) = expand' x
expand' (Mul [x,Add ys]) = sum $ map (expand' . ((*) x)) ys
expand' (Mul [x@(Add _),y]) = expand' (Mul [y,x])
expand' (Mul [x,y]) = simplifyProduct [x,y]
expand' (Mul (x:xs)) = do
                        y <- expand' (Mul xs)
                        expand' (Mul [x,y])

expand' (Pow x (Add ys)) = product $ map (expand' . (Pow x)) ys
expand' (Pow (Add []) n) = simplifyPow 0 n
expand' (Pow (Add [x]) n) = simplifyPow x n
expand' (Pow (Add (x:xs)) n)
    | n == 0 = 1
    | isInteger n && x < 0 = 1 / expand' (Pow (Add (x:xs)) (-n))
    | isInteger n && x > 0 = let
                                n' = floor $ fromNumber n
                                y = case xs of
                                        [z] -> z
                                        _ -> Add xs
                             in
                                 sum [ coeff n' k x y | k <- [0..n']]
        where
            coeff::Integer -> Integer -> PExpr -> PExpr -> Expr
            coeff m k a b = let
                                a' = expr a
                                b' = expr b
                                c = fromInteger (choose m k)
                                i = fromInteger k
                                j = fromInteger (m-k)
                            in
                                expand $ c * (a' ** i) * (b' ** j)
expand' (Pow x y) = do
                    x' <- expand' x
                    y' <- expand' y
                    simplifyPow x' y'
expand' (Fun f xs) = Fun f <$> mapM expand' xs
expand' x = return x
