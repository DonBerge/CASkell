module Simplification.Rationalize where

import PExpr
import Symplify
import Expr

rationalize :: PExpr -> Expr
rationalize (Pow x y) = rationalize x >>= (`simplifyPow` y)
rationalize (Mul xs) = mapM rationalize xs >>= simplifyProduct
rationalize (Add []) = 0
rationalize (Add (x:xs)) = do
                            g <- rationalize x
                            h <- rationalize (Add xs)
                            rationalizeSum g h
rationalize u = return u

rationalizeSum :: PExpr -> PExpr -> Expr
rationalizeSum u v = do
                        m <- numerator u
                        r <- denominator u
                        n <- numerator v
                        s <- denominator v
                        if r == 1 && s == 1
                            then simplifySum [u,v]
                            else do
                                    ms <- simplifyProduct [m,s]
                                    nr <- simplifyProduct [n,r]
                                    (rationalizeSum ms nr) / (simplifyProduct [r,s])
