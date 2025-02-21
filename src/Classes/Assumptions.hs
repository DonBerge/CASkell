-- |
-- Module      : NombreDelModulo
-- Description : Definicion de funciones para realizar suposiciones
-- A veces es necesario conocer ciertas propiedades sobre las expresiones para poder realizar evaluaciones o simplificaciones,
-- por ejemplo, @(x*y)^(1/2) = x^(1/2) * y^(1/2)@ solo si @x@ e @y@ son positivos
-- 
-- Este modulo permite definir una serie de funciones que permiten deducir estas propiedades. Soporta logica de 3 valores para el
-- caso donde se desconoce si la propiedad se cumple o no.

module Classes.Assumptions (
    Assumptions(..),
    module TriBool
) where

import TriBool

-- | Define funciones que permiten determinar si un cierto tipo de datos cumple con ciertas propiedades, 
-- como por ejemplo si es positivo, negativo, cero, par, impar, entero, etc.
class Assumptions a where
    isPositive :: a -> TriBool
    isPositive x = not3 (isNegative x ||| isZero x)

    isNegative :: a -> TriBool
    isNegative x = not3 (isPositive x ||| isZero x)

    isZero :: a -> TriBool
    isZero x = not3 (isPositive x ||| isNegative x)

    isEven :: a -> TriBool
    isEven x = isInteger x &&& not3 (isOdd x)

    isOdd :: a -> TriBool
    isOdd x = isInteger x &&& not3 (isEven x)

    isInteger :: a -> TriBool
    isInteger x = isEven x ||| isOdd x