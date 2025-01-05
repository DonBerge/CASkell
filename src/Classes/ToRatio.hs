module Classes.ToRatio where

class ToRatio a where
  numerator :: a -> a
  denominator :: a -> a