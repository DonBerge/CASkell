-- permite pasar funciones polimorficas como argumento
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Evaluate.Numeric
  ( eval,
  )
where

import Expr
import qualified Number as N
import Structure

-- $setup
-- >>> let x = symbol "x"

-- | Contexto de evaluación de expresiones.
type Context = [(Expr, N.Number)]

-- |
--    Evaluación de operaciones flotantes sobre números. f
--
--    === Ejemplos
--    
--    >>> evalFloatingOp sin 2  -- evalua sin(2)
--    0.9092974268256817
--    >>> evalFloatingOp sin x  -- evalua sin(x), como x no es un valor numeico, no se evalua.
--    Sin(x)    
evalFloatingOp :: (forall a. (Floating a) => a -> a) -> Expr -> Expr
evalFloatingOp f (structure -> Number n) = construct $ Number $ f n
evalFloatingOp f x = f x

{-|
  Elimina un par clave-valor de una lista de pares clave-valor.
-}
deleteWithKey :: (Eq a) => a -> [(a, b)] -> [(a, b)]
deleteWithKey _ [] = []
deleteWithKey x ((y, z) : xs)
  | x == y = xs
  | otherwise = (y, z) : deleteWithKey x xs

{-|
  Evalua numericamente la expresión dada, reemplazando los simbolos por sus valores en el contexto.

  === Ejemplos

  >>> eval [] (sqrt(2) * pi)
  4.442882938158366
  >>> eval [] (pi*x**2 + x/3)
  (1/3)*x+3.141592653589793*x^2
  >>> eval [(x,pi)] (2*sin(x/4))
  1.414213562373095
-}
eval :: Context -> Expr -> Expr
eval _ (structure -> Pi) = construct $ Number pi
eval ctx u@(structure -> Symbol _) = maybe u (construct . Number) $ lookup u ctx
eval ctx (structure -> Derivative u x) = construct $ Derivative (eval (deleteWithKey x ctx) u) x -- no evaluar la variable de derivacion
eval ctx (structure -> Integral u x) = construct $ Integral (eval (deleteWithKey x ctx) u) x -- no evaluar la variable de integracion
eval ctx (structure -> DefiniteIntegral u x a b) =
  let u' = eval (deleteWithKey x ctx) u
      a' = eval ctx a
      b' = eval ctx b
   in construct $ DefiniteIntegral u' x a' b' -- no evaluar la variable de integracion
eval ctx u = eval' $ mapStructure (eval ctx) u
  where
    -- evaluar funciones con numeros como argumento, ademas de potencias de numeros
    eval' (structure -> Pow a b)
      | Number n <- structure a, Number m <- structure b = construct $ Number $ n ** m -- potencias de numeros
    eval' (structure -> Sin x) = evalFloatingOp sin x -- uno de los sin evalua a numeros y otro a expresiones
    eval' (structure -> Cos x) = evalFloatingOp cos x
    eval' (structure -> Tan x) = evalFloatingOp tan x
    eval' (structure -> Asin x) = evalFloatingOp asin x
    eval' (structure -> Acos x) = evalFloatingOp acos x
    eval' (structure -> Atan x) = evalFloatingOp atan x
    eval' (structure -> Sinh x) = evalFloatingOp sinh x
    eval' (structure -> Cosh x) = evalFloatingOp cosh x
    eval' (structure -> Tanh x) = evalFloatingOp tanh x
    eval' (structure -> Asinh x) = evalFloatingOp asinh x
    eval' (structure -> Acosh x) = evalFloatingOp acosh x
    eval' (structure -> Atanh x) = evalFloatingOp atanh x
    eval' (structure -> Exp x) = evalFloatingOp exp x
    eval' (structure -> Log x) = evalFloatingOp log x
    eval' v = v