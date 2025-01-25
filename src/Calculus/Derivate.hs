{-|
Module      : Calculus.Derivate
Description : Derivar una expresion respecto a una variable

Detailed description of the module's functionality, including any important
details or usage examples.
-}
module Calculus.Derivate where 
import Symplify

import Control.Monad ((>=>))
  
simplifySqrt :: PExpr -> EvalSteps PExpr
simplifySqrt x = x `simplifyPow` (1/2)

notASymbol :: PExpr -> Bool
notASymbol (Symbol _) = False
notASymbol _ = True

derivate :: PExpr -> PExpr -> EvalSteps PExpr
derivate u x
    | notASymbol x = fail $ "La variable de derivacion(" ++ show u ++ ") no es un simbolo"
    | u == x = return 1
    | freeOf u x = return 0
derivate u@(Pow v w) x = do
                            dv <- derivate v x
                            dw <- derivate w x
                            vw' <- simplifySub w 1 >>= \subw -> simplifyPow v subw
                            a <- simplifyProduct [w, vw', dv]
                            b <- (simplifyFun . Log) v >>= \logv -> simplifyProduct [dw, u, logv]
                            simplifySum [a,b]  
derivate (Add us) x = mapM (`derivate` x) us >>= simplifySum
derivate (Mul []) _ = return 0
derivate (Mul (v:us)) x = do
                            let w = Mul us
                            dv <- derivate v x
                            dw <- derivate w x
                            a <- simplifyProduct [dv, w]
                            b <- simplifyProduct [v, dw]
                            simplifySum [a,b]
-- derivation of functions
derivate u@(Fun _ [v]) x = do
                            df <- derivateFun u
                            dv <- derivate v x
                            simplifyProduct [df, dv]
derivate u x = fail $ "No se puede derivar la expresion " ++ show u ++ " respecto a " ++ show x

derivateFun :: PExpr -> EvalSteps PExpr
derivateFun (Sin v) = simplifyFun . Cos $ v -- cos(x)
derivateFun (Cos v) = simplifyFun . Sin >=> simplifyNegate $ v -- -sin(x)
derivateFun (Tan v) = simplifyFun . Tan >=> (`simplifyPow` 2) >=> simplifySum . (:[1]) $ v -- 1 + tan^2(x) = sec^2(x), derivada de tan(x) = sec^2(x)
derivateFun (Cot v) = simplifyFun . Cot >=> (`simplifyPow` 2) >=> simplifySum . (:[1]) >=> simplifyNegate $ v -- -1 - cot^2(x) = -csc^2(x), derivada de cot(x) = -csc^2(x)
derivateFun (Sec v) = do
                        tg <- simplifyFun . Tan $ v
                        sc <- simplifyFun . Sec $ v
                        simplifyProduct [tg, sc] -- tan(x)sec(x)
derivateFun (Csc v) = do
                        ct <- simplifyFun . Cot $ v
                        csc <- simplifyFun . Csc $ v
                        simplifyProduct [ct, csc] >>= simplifyNegate -- -cot(x)csc(x)
derivateFun (Asin v) = simplifyPow v 2 >>= simplifySub 1 >>= simplifySqrt >>= simplifyDiv 1 -- 1/(1-x^2)^(1/2)
derivateFun (Acos v) = simplifyPow v 2 >>= simplifySub 1 >>= simplifySqrt >>= simplifyDiv 1 >>= simplifyNegate -- -1/(1-x^2)^(1/2) 
derivateFun (Atan v) = simplifyPow v 2 >>= simplifySum . (:[1]) >>= simplifyDiv 1 -- 1/(1+x^2)
derivateFun (Sinh v) = simplifyFun . Cosh $ v -- cosh(x)
derivateFun (Cosh v) = simplifyFun . Sinh $ v -- sinh(x)
derivateFun (Tanh v) = (simplifyFun . Cosh) v >>= (`simplifyPow` 2) >>= simplifyDiv 1  -- sech^2(x)
derivateFun (Asinh v) = simplifyPow v 2 >>= simplifySum . (:[1]) >>= simplifySqrt >>= simplifyDiv 1 -- 1/(1+x^2)^(1/2)
derivateFun (Acosh v) = simplifyPow v 2 >>= (`simplifySub` 1) >>= simplifySqrt >>= simplifyDiv 1 -- 1/(x^2-1)^(1/2)
derivateFun (Atanh v) = simplifyPow v 2 >>= simplifySub 1 >>= simplifyDiv 1 -- 1/(1-x^2)
derivateFun (Exp v) = return $ Exp v
derivateFun (Log v) = simplifyDiv 1 v
derivateFun u = fail $ "Se desconoce la derivada de la funcion " ++ show u