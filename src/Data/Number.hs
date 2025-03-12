{-# OPTIONS_GHC -Wall #-}

{-|
Module: Data.Number
Description: Definición y operaciones para el tipo 'Number', que puede representar enteros, fracciones y números reales.

Este módulo define el tipo 'Number' y proporciona varias operaciones y clases de instancias para trabajar con números que pueden ser enteros, fracciones o números reales.
-}
module Data.Number (
    -- * Tipos
    Number,
    -- * Funciones
    numerator,
    denominator,
    printAsFraction
) where

import qualified Data.Ratio as R

import Assumptions
import Data.Bifunctor (Bifunctor(bimap))

-- | El tipo 'Number' representa un número que puede ser un entero, una fracción o un número real.
newtype Number = Number { fromNumber :: Rational } deriving (Eq, Ord)

-- | Obtiene la cantidad de dígitos de un número entero.
digitCount :: Integer -> Int
digitCount = length . show . abs

-- | "maxDigits" es la cantidad máxima de dígitos que para que un numero se muestre como fraccion.
maxDigits :: Int
maxDigits = 7

-- | Convierte un 'Number' a un numero de punto flotante.
toDouble :: Number -> Double
toDouble (Number x) = fromRational x

printAsFraction :: Number -> Bool
printAsFraction x = dn <= maxDigits && dd <= maxDigits && d/=1
    where
        n = R.numerator $ fromNumber x
        d = R.denominator $ fromNumber x
        dn = digitCount $ n
        dd = digitCount $ d

instance Show Number where
-- Mostrar un numero racional como fraccion si tiene pocos digitos, sino mostrarlo como un double
  show x
    | true (isInteger x) = show n
    | printAsFraction x = show n ++ "/" ++ show d
    | otherwise = show $ toDouble x
    where
        n = R.numerator $ fromNumber x
        d = R.denominator $ fromNumber x

instance Num Number where
    Number a + Number b = Number (a+b)
    Number a * Number b = Number (a*b)
    negate (Number a) = Number (-a)
    abs (Number a) = Number (abs a)
    signum (Number a) = Number (signum a)
    fromInteger = Number . fromInteger

instance Fractional Number where
    Number a / Number b = Number (a/b)
    recip (Number a) = Number (recip a)
    fromRational = Number

instance Real Number where
    toRational = fromNumber

instance RealFrac Number where
    properFraction x = (fromInteger n, Number f)
        where
            (n,f) = properFraction $ fromNumber x

lift :: (Double -> Double) -> Number -> Number
lift f =  Number . toRational . f . toDouble

instance Floating Number where
    pi = Number $ toRational (pi :: Double)

    (**) x = lift ((**) (toDouble x))

    exp = lift exp
    log = lift log
    sqrt = lift sqrt
    sin = lift sin
    cos = lift cos
    tan = lift tan
    asin = lift asin
    acos = lift acos
    atan = lift atan
    sinh = lift sinh
    cosh = lift cosh
    tanh = lift tanh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh

-- | Obtiene el numerador de un 'Number'.
numerator :: Number -> Integer
numerator = R.numerator . fromNumber

-- | Obtiene el denominador de un 'Number'.
denominator :: Number -> Integer
denominator = R.denominator . fromNumber

instance Assumptions Number where
    isPositive x = liftBool $ x > 0
    isZero x = liftBool $ x == 0
    isInteger x = liftBool $ denominator x == 1
    isEven x = isInteger x &&& even (numerator x)

instance Enum Number where
    toEnum = fromIntegral
    fromEnum x
        | true (isInteger x) = fromIntegral $ numerator x
        | otherwise = error "fromEnum: Not an Int"

instance Integral Number where
    quotRem a b
        | true (isInteger a &&& isInteger b) = bimap fromInteger fromInteger (quotRem (toInteger a) (toInteger b))
        | otherwise = error "quotRem: Not an Int"

    toInteger x
        | true (isInteger x) = numerator x
        | otherwise = error "toInteger: Not an Int"