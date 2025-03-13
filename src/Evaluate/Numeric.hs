-- permite pasar funciones polimorficas como argumento
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Evaluate.Numeric
-- Description : Evaluación de expresiones numéricas.
module Evaluate.Numeric
  ( eval,
  )
where

import Expr
import Data.Number
import Calculus.Derivate (makeUnevaluatedDerivative)
import Calculus.Integrate (makeUnevaluatedIntegral, makeUnevaluatedDefiniteIntegral)

-- $setup
-- >>> let x = symbol "x"

-- | Contexto de evaluación de expresiones.
type Context = [(Expr, Number)]

-- |
--    Evaluación de operaciones flotantes sobre números. f
--
--    === Ejemplos
--
--    >>> evalFloatingOp sin 2  -- evalua sin(2)
--    0.9092974268256817
--    >>> evalFloatingOp sin x  -- evalua sin(x), como x no es un valor numeico, no se evalua.
--    sin(x)
evalFloatingOp :: (forall a. (Floating a) => a -> a) -> Expr -> Expr
evalFloatingOp f (Number n) = fromNumber $ f n
evalFloatingOp f x = f x

-- |
--  Elimina un par clave-valor de una lista de pares clave-valor.
deleteWithKey :: (Eq a) => a -> [(a, b)] -> [(a, b)]
deleteWithKey _ [] = []
deleteWithKey x ((y, z) : xs)
  | x == y = xs
  | otherwise = (y, z) : deleteWithKey x xs

-- |
--  Evalua numericamente la expresión dada, reemplazando los simbolos por sus valores en el contexto.
--
--  === Ejemplos
--
--  >>> eval [] (sqrt(2) * pi)
--  4.442882938158366
--  >>> eval [] (pi*x**2 + x/3)
--  3.141592653589793*x^2+x/3
--  >>> eval [(x,pi)] (2*sin(x/4))
--  1.414213562373095
eval :: Context -> Expr -> Expr
eval _ Pi = fromNumber pi
eval ctx u@(Symbol _) = maybe u fromNumber $ lookup u ctx
eval ctx (Derivative u x) = makeUnevaluatedDerivative (eval (deleteWithKey x ctx) u) x -- no evaluar la variable de derivacion
eval ctx (Integral u x) = makeUnevaluatedIntegral (eval (deleteWithKey x ctx) u) x -- no evaluar la variable de integracion
eval ctx (DefiniteIntegral u x a b) =
  let u' = eval (deleteWithKey x ctx) u
      a' = eval ctx a
      b' = eval ctx b
   in makeUnevaluatedDefiniteIntegral u' x a' b' -- no evaluar la variable de integracion
eval ctx u = eval' $ mapStructure (eval ctx) u
  where
    -- evaluar funciones con numeros como argumento, ademas de potencias de numeros
    eval' (Pow (Number n) (Number m)) = fromNumber $ n ** m -- potencias de numeros
    eval' (Sin x) = evalFloatingOp sin x -- uno de los sin evalua a numeros y otro a expresiones
    eval' (Cos x) = evalFloatingOp cos x
    eval' (Asin x) = evalFloatingOp asin x
    eval' (Acos x) = evalFloatingOp acos x
    eval' (Atan x) = evalFloatingOp atan x
    eval' (Exp x) = evalFloatingOp exp x
    eval' (Log x) = evalFloatingOp log x
    eval' v = v