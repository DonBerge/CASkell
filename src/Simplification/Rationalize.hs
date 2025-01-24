module Simplification.Rationalize where
import Symplify

rationalize :: PExpr -> EvalSteps PExpr
rationalize (Pow x y) = rationalize x >>= (`simplifyPow` y)
rationalize (Mul xs) = mapM rationalize xs >>= simplifyProduct
rationalize (Add []) = return 0
rationalize (Add (x:xs)) = do
                            g <- rationalize x
                            h <- rationalize (Add xs)
                            rationalizeSum g h
rationalize u = return u
rationalizeSum :: PExpr -> PExpr -> EvalSteps PExpr

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
                                    n' <- rationalizeSum ms nr
                                    d' <- simplifyProduct [r,s]
                                    simplifyDiv n' d'
                                    -- (rationalizeSum ms nr) / (simplifyProduct [r,s])
