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
    isNegative :: a -> TriBool
    isZero :: a -> TriBool
    isEven :: a -> TriBool
    isOdd :: a -> TriBool
    isInteger :: a -> TriBool