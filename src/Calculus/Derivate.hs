{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Calculus.Derivate
Description : Derivar una expresion respecto a una variable

Detailed description of the module's functionality, including any important
details or usage examples.
-}
module Calculus.Derivate where 

import Expr
import Structure

notAVariable :: Expr -> Bool
notAVariable (structure -> Pi) = True
notAVariable (structure -> Symbol _) = False
notAVariable _ = True

makeUnevaluatedDerivative :: Expr -> Expr -> Expr
makeUnevaluatedDerivative u = construct . Derivative u

derivate :: Expr -> Expr -> Expr
derivate u x
    | notAVariable x = fail $ "Derivate: " ++ show x ++ " no es una variable"
    | u == x = 1
    | freeOf u x = 0
derivate u@(structure -> Pow v w) x = let
                                        dv = derivate v x
                                        dw = derivate w x
                                      in
                                        w * v**(w-1) * dv + dw * u * log v
derivate u@(structure -> Add _) x = mapStructure (`derivate` x) u -- addStep (du/dx) (suma derivadas) [u, du1, du2, ...]
derivate u@(structure -> Mul us) x = sum $ fmap ((u*) . logder) us
    where
        logder f = (f `derivate` x) / f
derivate u@(structure -> Fun _ (v:|[])) x = let
                                              df = derivateFun u v -- TODO VER ESTO
                                              dv = derivate v x
                                            in
                                              df * dv
-- Derivada de una integral
derivate (structure -> Integral v y) x
    | x == y = v
-- Derivada desconocida, devolver una derivada sin evaluar
derivate u x = makeUnevaluatedDerivative u x


derivateFun :: Expr -> Expr -> Expr
derivateFun (structure -> Sin v) _ = cos v
derivateFun (structure -> Cos v) _ = negate $ sin v
derivateFun (structure -> Tan v) _ = 1/(cos v ** 2)
derivateFun (structure -> Cot v) _ = negate $ 1 + cot v ** 2
derivateFun (structure -> Sec v) _ = tan v * sec v
derivateFun (structure -> Csc v) _ = negate $ cot v * csc v
derivateFun (structure -> Asin v) _ = 1 / sqrt (1 - v ** 2)
derivateFun (structure -> Acos v) _ = negate $ 1 / sqrt (1 - v ** 2)
derivateFun (structure -> Atan v) _ = 1 / (1 + v ** 2)
derivateFun (structure -> Asinh v) _ = 1 / sqrt (1 + v ** 2)
derivateFun (structure -> Acosh v) _ = 1 / sqrt (v ** 2 - 1)
derivateFun (structure -> Atanh v) _ = 1 / (1 - v ** 2)
derivateFun (structure -> Sinh v) _ = cosh v
derivateFun (structure -> Cosh v) _ = sinh v
derivateFun (structure -> Tanh v) _ = 1 / (cosh v ** 2)
derivateFun (structure -> Exp v) _ = exp v
derivateFun (structure -> Log v) _ = 1 / v
-- Derivada desconocida, devolver un operador sin evaluar
derivateFun u x = makeUnevaluatedDerivative u x