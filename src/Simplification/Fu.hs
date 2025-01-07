module Simplification.Fu where

import PExpr

import Symplify

import Expr

bottomUp :: (PExpr -> Expr) -> PExpr -> Expr
bottomUp f (Add xs) = (mapM (bottomUp f) xs) >>= simplifySum >>= f
bottomUp f (Mul xs) = (mapM (bottomUp f) xs) >>= simplifyProduct >>= f
bottomUp f (Pow x y) = do
                        x' <- bottomUp f x
                        y' <- bottomUp f y
                        simplifyPow x' y' >>= f
bottomUp f (Fun g xs) = (mapM (bottomUp f) xs) >>= f . Fun g
bottomUp _ x = return x

tr0 :: PExpr -> Expr
tr0 = return

tr1 :: PExpr -> Expr
tr1 = bottomUp tr1'
    where
        tr1' (Sec x) = 1 / cos (return x)
        tr1' (Csc x) = 1 / sin (return x)
        tr1' x = return x

tr2 :: PExpr -> Expr
tr2 = bottomUp tr2'
    where
        tr2' (Tan x) = let x' = return x in sin (x') / cos (x')
        tr2' (Cot x) = let x' = return x in cos (x') / sin (x')
        tr2' x = return x

tr2i :: PExpr -> Expr
tr2i = bottomUp tr2i'
    where
        tr2i' x = do
                    n <- numerator x
                    d <- denominator x
                    case (n,d) of
                        (Sin a, Cos b) | a==b -> tan $ return a
                        (Cos a, Sin b) | a==b -> cot $ return a
                        _ -> return x

tr3 :: PExpr -> Expr
tr3 = bottomUp tr3'
    where
        tr3' (Sin x)
            | isTrue $ isNegative x = let x' = return x in -sin (-x')
        -- falta pi-x, pi+x, 2kpi+x, etc   
        tr3' (Cos x)
            | isTrue $ isNegative x = let x' = return x in cos (-x')
        
        tr3' (Tan x)
            | isTrue $ isNegative x = let x' = return x in -tan (-x')
        
        tr3' (Cot x)
            | isTrue $ isNegative x = let x' = return x in -cot (-x')
        tr3' x = return x

tr4 :: PExpr -> Expr
tr4 = return -- ya es manejado por autosimplificacion

tr5 :: PExpr -> Expr
tr5 = bottomUp tr5'
    where
        tr5' (Pow (Sin x) 2) = let x' = return x in 1-cos x' ** 2
        tr5' x = return x

tr6 :: PExpr -> Expr
tr6 = bottomUp tr5'
    where
        tr5' (Pow (Cos x) 2) = let x' = return x in 1-sin x' ** 2
        tr5' x = return x

tr7 :: PExpr -> Expr
tr7 = bottomUp tr7'
    where
        tr7' (Pow (Cos x) 2) = let x' = return x in (1+cos(2*x'))/2
        tr7' x = return x

tr8 :: PExpr -> Expr
tr8 = bottomUp tr8'
    where
        tr8' (Mul []) = 1
        tr8' (Mul ((Sin x):(Cos y):xs)) = let
                                            x' = return x
                                            y' = return y
                                          in
                                            ((1/2) * (sin (x'+y') + sin (x' - y'))) * (tr8' (Mul xs))
        tr8' (Mul ((Cos x):(Sin y):xs)) = let
                                            x' = return x
                                            y' = return y
                                          in
                                            ((1/2) * (sin (x'+y') - sin (x' - y'))) * (tr8' (Mul xs))
        tr8' (Mul ((Cos x):(Cos y):xs)) = let
                                            x' = return x
                                            y' = return y
                                          in
                                            ((1/2) * (cos (x'+y') + cos (x' - y'))) * (tr8' (Mul xs))
        tr8' (Mul ((Sin x):(Sin y):xs)) = let
                                            x' = return x
                                            y' = return y
                                          in
                                            ((1/2) * (cos (x'+y') - cos (x' - y'))) * (tr8' (Mul xs))
        tr8' (Mul (Sin x:y:xs)) = let y' = return y in y' * (tr8' (Mul ((Sin x):xs)))
        tr8' (Mul (Cos x:y:xs)) = let y' = return y in y' * (tr8' (Mul ((Cos x):xs)))
        tr8' (Mul (x:xs)) = let x' = return x in x' * (tr8' (Mul xs))
        tr8' x = return x

tr9 :: PExpr -> Expr
tr9 = bottomUp tr9'
    where
        tr9' (Add []) = 0
        tr9' (Add ((Sin x):(Cos y):xs)) = let
                                            x' = return x
                                            y' = return y
                                          in 2 * sin ((x'+y')/2) * cos ((x'-y')/2) * tr9' (Add xs)
        tr9' x = return x