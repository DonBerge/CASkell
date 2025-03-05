{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Calculus.Derivate
-- Description : Proporciona funcionalidad para calcular la derivada de expresiones matemáticas.
--
-- Este módulo define funciones para calcular la derivada de expresiones matemáticas con respecto a una variable dada. Soporta la diferenciación de operaciones aritméticas básicas, funciones trigonométricas, funciones hiperbólicas, funciones exponenciales y logarítmicas, y también maneja la diferenciación de integrales y funciones desconocidas devolviendo derivadas no evaluadas.
module Calculus.Derivate where

import Expr

import Calculus.Utils (notAVariable)

-- $setup
-- >>> let x = symbol "x"
-- >>> let y = symbol "y"
-- >>> let z = symbol "z"
-- >>> let u = symbol "u"
-- >>> let f = function "f"
-- >>> let g = function "g"

-- * Patron de derivada
pattern Derivative :: Expr -> Expr -> Expr
pattern Derivative u x <- Fun "Derivate" (u :| [x]) --(matchAnyarityFun "Derivative" -> Just (u :| [x]))

-- |
--  Construye una derivada sin evaluar
--
-- >>> makeUnevaluatedDerivative u x
-- Derivate(u,x)
makeUnevaluatedDerivative :: Expr -> Expr -> Expr
makeUnevaluatedDerivative u x = function "Derivate" [u,x]

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
--
--  === Ejemplos
--
--  >>> derivate (x**2 + 2*x + 1) x
--  2+2*x
--  >>> derivate (sin x) x
--  Cos(x)
--  >>> derivate (exp(x**2)) x
--  2*Exp(x^2)*x
--  >>> derivate (x*log(x)-x) x
--  Log(x)
--  >>> derivate (f[x]) x 
--  Derivate(f(x),x)
--  >>> derivate (f[x]*exp(x)) x 
--  Derivate(f(x),x)*Exp(x)+Exp(x)*f(x)
--  >>> derivate (f[g[x]]) x
--  Derivate(f(g(x)),g(x))*Derivate(g(x),x)
derivate :: Expr -> Expr -> Expr
derivate u x
  | notAVariable x = undefinedExpr $ "Derivate: " ++ show x ++ " no es una variable"
  | u == x = 1 -- derivada de x
  | freeOf u x = 0 -- derivada de una constante
derivate u@(Pow v w) x =
  let dv = derivate v x
      dw = derivate w x
   in w * v ** (w - 1) * dv + dw * u * log v -- regla de la potencia
derivate u@(Add _) x = mapStructure (`derivate` x) u -- regla de la suma
derivate u@(Mul us) x = sum $ fmap ((u *) . logder) us -- regla del producto
  where
    logder f = (f `derivate` x) / f -- logder f = (log(f(x)))' = f'(x)/f(x)
derivate u@(Fun _ (v :| [])) x =
  let df = derivateTable u -- derivar u respecto de v
      dv = derivate v x
   in df * dv -- regla de la cadena
  -- Derivada de una integral
derivate (Integral v y) x
  | x == y = v
-- Derivada desconocida, devolver una derivada sin evaluar
derivate u x = makeUnevaluatedDerivative u x

-- |
--    'derivateTable' es una tabla de derivadas que contiene las derivadas de las funciones matemáticas más comunes.
--
--    Si la derivada de una función no está en la tabla, se devuelve una derivada sin evaluar.
--
--    Si el argumento pasado no es una función, se devuelve Undefined.
--
--    Ejemplos:
--
--      >>> derivateTable (sin x)
--      Cos(x)
--      >>> derivateTable (exp x)
--      Exp(x)
--      >>> derivateTable (f[x])
--      Derivate(f(x),x)
derivateTable :: Expr -> Expr
derivateTable (Sin v) = cos v
derivateTable (Cos v) = negate $ sin v
derivateTable (Tan v) = 1 / (cos v ** 2)
derivateTable (Cot v) = negate $ 1 + cot v ** 2
derivateTable (Sec v) = tan v * sec v
derivateTable (Csc v) = negate $ cot v * csc v
derivateTable (Asin v) = 1 / sqrt (1 - v ** 2)
derivateTable (Acos v) = negate $ 1 / sqrt (1 - v ** 2)
derivateTable (Atan v) = 1 / (1 + v ** 2)
derivateTable (Asinh v) = 1 / sqrt (1 + v ** 2)
derivateTable (Acosh v) = 1 / sqrt (v ** 2 - 1)
derivateTable (Atanh v) = 1 / (1 - v ** 2)
derivateTable (Sinh v) = cosh v
derivateTable (Cosh v) = sinh v
derivateTable (Tanh v) = 1 / (cosh v ** 2)
derivateTable (Exp v) = exp v
derivateTable (Log v) = 1 / v
-- Derivada desconocida, devolver un operador sin evaluar
derivateTable u@(Fun _ (v :| [])) = makeUnevaluatedDerivative u v
derivateTable _ = undefinedExpr "DerivateTable: El argumento no es una funcion"