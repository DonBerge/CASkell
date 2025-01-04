module Classes.Assumptions where

import TriBool

class Assumptions a where
    isPositive :: a -> TriBool
    isNegative :: a -> TriBool
    isZero :: a -> TriBool
    isEven :: a -> TriBool
    isOdd :: a -> TriBool
    isInteger :: a -> TriBool