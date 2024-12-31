module Algebraic where

import Expr
import PExpr

import Symplify

expand :: PExpr -> Expr
expand (Add xs) = mapM expand xs >>= simplifySum
expand (Mul []) = 1
expand (Mul [x]) = expand x
expand (Mul [x,Add ys]) = mapM (expand . ((*) x)) ys >>= simplifySum
expand (Mul [x@(Add _),y]) = expand (Mul [y,x])
expand (Mul [x,y]) = simplifyProduct [x,y]
expand (Mul (x:xs)) = do
                        y <- expand (Mul xs)
                        expand (Mul [x,y])

expand (Pow x (Add ys)) = mapM (expand . (Pow x)) ys >>= simplifyProduct
expand (Pow x y) = do
                    x' <- expand x
                    y' <- expand y
                    simplifyPow x' y'
expand (Fun f xs) = Fun f <$> mapM expand xs
expand x = return x
