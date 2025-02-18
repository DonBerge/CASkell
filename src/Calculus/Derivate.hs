{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Calculus.Derivate
-- Description : Proporciona funcionalidad para calcular la derivada de expresiones matemáticas.
--
-- Este módulo define funciones para calcular la derivada de expresiones matemáticas con respecto a una variable dada. Soporta la diferenciación de operaciones aritméticas básicas, funciones trigonométricas, funciones hiperbólicas, funciones exponenciales y logarítmicas, y también maneja la diferenciación de integrales y funciones desconocidas devolviendo derivadas no evaluadas.
module Calculus.Derivate where

import Expr
import Structure

-- | Verifica si una expresión dada es una variable
notAVariable :: Expr -> Bool
notAVariable (structure -> Pi) = True
notAVariable (structure -> Symbol _) = False
notAVariable _ = True

-- |
--  Construye una derivada sin evaluar
--
--  > structure (makeUnevaluatedDerivative u x) = Derivate u x
makeUnevaluatedDerivative :: Expr -> Expr -> Expr
makeUnevaluatedDerivative u = construct . Derivative u

-- |
--  Utiliza las reglas de derivación para calcular la derivada de una expresión con respecto a una variable dada.
--
--  Las reglas de derivación utilizadas son:
--
--    * Derivada de una constante: \(\dfrac{d}{dx}c = 0\)
--    * Regla de la suma: \(\dfrac{d}{dx}\sum u_i = \sum \dfrac{du_i}{dx}\)
--    * Regla del producto: \( \dfrac{d}{dx}(u \cdot v) = u \cdot \dfrac{dv}{dx} + v \cdot \dfrac{du}{dx} \), mas especificamente,
--      la forma general de dicha regla, \(\displaystyle \dfrac{d}{dx}\left(\prod u_i\right) = \left(\prod u_i\right) \cdot \sum \dfrac{du_i}{dx} \cdot \dfrac{1}{u_i} \)
--    * Regla de la potencia: \(\dfrac{d}{dx} v^w = w v^{w-1} \dfrac{dv}{dx} + \dfrac{dw}{dx}  v^w  \log v\)
--    * Regla de la cadena: \(\dfrac{d}{dx} f(g(x)) = f'(g(x)) \cdot g'(x)\)
--
--  Ademas se utliza la funciñon 'derivateTable' para calcular la derivada de funciones matemáticas comunes.
--
--  Si al aplicar las reglas de derivación no se puede calcular la derivada, se devuelve una derivada sin evaluar.
--  === Ejemplos
--  > derivate (x^2 + 2*x + 1) x = 2*x + 2
--  > derivate (sin x) x = cos x
--  > derivate (exp(x**2)) x = 2 * exp(x**2) * x
--  > derivate (x*log(x)-x) x = log x
--  > derivate (f(x)) x = Derivate(f(x),x) -- derivada sin evaluar
--  > derivate (f(x)*exp(x)) x = Derivate(f(x),x)*exp(x) + f(x)*exp(x) -- regla del producto sin evaluar
--  > derivate (f(g(x))) x = Derivate(f(g(x)), g(x)) * Derivate(g(x),x) -- regla de la cadena
derivate :: Expr -> Expr -> Expr
derivate u x
  | notAVariable x = fail $ "Derivate: " ++ show x ++ " no es una variable"
  | u == x = 1 -- derivada de x
  | freeOf u x = 0 -- derivada de una constante
derivate u@(structure -> Pow v w) x =
  let dv = derivate v x
      dw = derivate w x
   in w * v ** (w - 1) * dv + dw * u * log v -- regla de la potencia
derivate u@(structure -> Add _) x = mapStructure (`derivate` x) u -- regla de la suma
derivate u@(structure -> Mul us) x = sum $ fmap ((u *) . logder) us -- regla del producto
  where
    logder f = (f `derivate` x) / f -- logder f = (log(f(x)))' = f'(x)/f(x)
derivate u@(structure -> Fun _ (v :| [])) x =
  let df = derivateTable u v
      dv = derivate v x
   in df * dv -- regla de la cadena
  -- Derivada de una integral
derivate (structure -> Integral v y) x
  | x == y = v
-- Derivada desconocida, devolver una derivada sin evaluar
derivate u x = makeUnevaluatedDerivative u x

-- |
--    'derivateTable' es una tabla de derivadas que contiene las derivadas de las funciones matemáticas más comunes.
--
--    Si la derivada de una función no está en la tabla, se devuelve una derivada sin evaluar.
--
--    El segundo argumento solo se usa para construir la derivada sin evaluar.
--
--    Ejemplos:
--
--      > derivateTable (sin x) x = cos x
--      > derivateTable (exp x) z = exp x
--      > derivateTable (f(x)) = Derivate f x
derivateTable :: Expr -> Expr -> Expr
derivateTable (structure -> Sin v) _ = cos v
derivateTable (structure -> Cos v) _ = negate $ sin v
derivateTable (structure -> Tan v) _ = 1 / (cos v ** 2)
derivateTable (structure -> Cot v) _ = negate $ 1 + cot v ** 2
derivateTable (structure -> Sec v) _ = tan v * sec v
derivateTable (structure -> Csc v) _ = negate $ cot v * csc v
derivateTable (structure -> Asin v) _ = 1 / sqrt (1 - v ** 2)
derivateTable (structure -> Acos v) _ = negate $ 1 / sqrt (1 - v ** 2)
derivateTable (structure -> Atan v) _ = 1 / (1 + v ** 2)
derivateTable (structure -> Asinh v) _ = 1 / sqrt (1 + v ** 2)
derivateTable (structure -> Acosh v) _ = 1 / sqrt (v ** 2 - 1)
derivateTable (structure -> Atanh v) _ = 1 / (1 - v ** 2)
derivateTable (structure -> Sinh v) _ = cosh v
derivateTable (structure -> Cosh v) _ = sinh v
derivateTable (structure -> Tanh v) _ = 1 / (cosh v ** 2)
derivateTable (structure -> Exp v) _ = exp v
derivateTable (structure -> Log v) _ = 1 / v
-- Derivada desconocida, devolver un operador sin evaluar
derivateTable u v = makeUnevaluatedDerivative u v