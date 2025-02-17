{-# OPTIONS_GHC -Wall #-}

{-|
Module: Number
Description: Definición y operaciones para el tipo 'Number', que puede representar enteros, fracciones y números reales.
-}
module Number (
    -- * Tipos
    Number(..),
    -- * Funciones
    numerator,
    denominator,
    fromNumber
) where

import Data.Ratio ((%))
import qualified Data.Ratio as R

import Classes.Assumptions
import Data.Bifunctor (Bifunctor(bimap))

-- | El tipo 'Number' puede representar un entero, una fracción o un número real.
data Number = Int Integer | Fraction Rational | Real Double

digitCount :: Integer -> Int
digitCount = length . show . abs

epsilon :: Double
epsilon = 2**(-52)

maxDigits :: Int
maxDigits = 7

-- | Simplifica un 'Number' transformándolo entre los distintos tipos si es necesario.
simplify :: Number -> Number
simplify (Fraction x) 
    | R.denominator x == 1 = Int $ R.numerator x
    | dn <= maxDigits && dd <= maxDigits = Fraction x
    | otherwise = Real $ fromRational x
    where
        dn = digitCount $ R.numerator x
        dd = digitCount $ R.denominator x
-- Un real con pocos decimales se convierte en fraccion
simplify (Real x)
    | dn <= maxDigits && dd <= maxDigits = if R.denominator f == 1 
                                                then Int (R.numerator f) 
                                                else Fraction f -- simplify $ Fraction f
    | otherwise = Real x
    where
        f = R.approxRational x epsilon
        dn = digitCount $ R.numerator f
        dd = digitCount $ R.denominator f
simplify x = x

-- | Convierte un 'Number' a una fracción.
toFraction :: Number -> Number
toFraction (Int n) = Fraction (n%1)
toFraction (Real n) = Fraction (R.approxRational n epsilon)
toFraction x = x

-- | Convierte un 'Number' a un número real.
toReal :: Number -> Number
toReal (Int n) = Real (fromIntegral n)
toReal (Fraction n) = Real (fromRational n)
toReal x = x

-- | Función auxiliar para aplicar operaciones entre dos 'Number'.
makeOp :: (Number -> Number -> t) -> Number -> Number -> t
makeOp op a@(Int _) b@(Int _) = a `op` b
makeOp op a@(Int _) b@(Fraction _) = toFraction a `op` b
makeOp op a@(Int _) b@(Real _) = toReal a `op` b
makeOp op a@(Fraction _) b@(Int _) = a `op` toFraction b
makeOp op a@(Fraction _) b@(Fraction _) = a `op` b
makeOp op a@(Fraction _) b@(Real _) = toReal a `op` b
makeOp op a@(Real _) b = a `op` toReal b

-- | Instancia de igualdad para 'Number'.
instance Eq Number where
  Int a == Int b = a==b
  Fraction a == Fraction b = a==b
  Real a == Real b = a==b

  a==b = makeOp (==) a b

-- | Instancia de orden para 'Number'.
instance Ord Number where
    Int a <= Int b = a<=b
    Fraction a <= Fraction b = a<=b
    Real a <= Real b = a<=b
    
    a<=b = makeOp (<=) a b

-- | Instancia de 'Show' para 'Number'.
instance Show Number where
  show (Int n) = show n
  show (Fraction n) = show (R.numerator n) ++ "/" ++ show (R.denominator n)
  show (Real n) = show n

-- | Instancia de 'Num' para 'Number'.
instance Num Number where
    fromInteger = Int

    ---
    Int a + Int b = Int (a+b)
    Fraction a + Fraction b = simplify $ Fraction (a+b)
    Real a + Real b = simplify $ Real (a+b)

    a+b = makeOp (+) a b

    ---
    Int a * Int b = Int (a*b)
    Fraction a * Fraction b = simplify $ Fraction (a*b)
    Real a * Real b = simplify $ Real (a*b)
    a*b = makeOp (*) a b
    --
    negate (Int a) = Int (-a)
    negate (Fraction a) = Fraction (-a)
    negate (Real a) = Real (-a)
    ---
    abs (Int a) = Int (abs a)
    abs (Fraction a) = Fraction (abs a)
    abs (Real a) = Real (abs a)

    ---
    signum (Int a) = Int (signum a)
    signum (Fraction a) = Int $ R.numerator $ signum a
    signum (Real a) = Int $ truncate $ signum a

-- | Instancia de 'Fractional' para 'Number'.
instance Fractional Number where
    fromRational = simplify . Fraction

    recip (Int a) = simplify $ Fraction (1%a)
    recip (Fraction a) = simplify $ Fraction $ recip a
    recip (Real a) = simplify $ Real $ recip a

makeFun :: (Double -> Double) -> Number -> Number
makeFun f (Real x) = simplify $ Real $ f x
makeFun f x = makeFun f $ toReal x

-- | Instancia de 'Floating' para 'Number'.
instance Floating Number where
    pi = Real pi
    exp = makeFun exp
    log = makeFun log
    sqrt = makeFun sqrt
    sin = makeFun sin
    cos = makeFun cos
    tan = makeFun tan
    asin = makeFun asin
    acos = makeFun acos
    atan = makeFun atan
    sinh = makeFun sinh
    cosh = makeFun cosh
    tanh = makeFun tanh
    asinh = makeFun asinh
    acosh = makeFun acosh
    atanh = makeFun atanh

    x ** (Int y) = x ^^ y
    x ** y@(Fraction _) = x ** toReal y
    (Int x) ** (Real y) = simplify $ Real (fromIntegral x ** y)
    (Fraction x) ** (Real y) = simplify $ Real (fromRational x ** y)
    (Real x) ** (Real y) = simplify $ Real (x ** y)

-- | Instancia de 'Real' para 'Number'.
instance Real Number where
    toRational (Fraction a) = a
    toRational x = toRational $ toFraction x

-- | Instancia de 'RealFrac' para 'Number'.
instance RealFrac Number where
    properFraction (Int i) = (fromInteger i, 0)
    properFraction (Fraction x) = let
                                    (n,f) = properFraction x
                                  in
                                    (n, simplify $ Fraction f)
    properFraction (Real x) = let
                                (n,f) = properFraction x
                              in
                                (n, simplify $ Real f)


--- NOTA: Asume que el numero esta simplificado
-- | Instancia de 'Assumptions' para 'Number'.
instance Assumptions Number where
    isPositive (Int x) = liftBool $ x > 0
    isPositive (Fraction x) = liftBool $ x > 0
    isPositive (Real x) = liftBool $ x > 0

    isNegative (Int x) = liftBool $ x < 0
    isNegative (Fraction x) = liftBool $ x < 0
    isNegative (Real x) = liftBool $ x < 0

    isZero (Int x) = liftBool $ x == 0
    isZero _ = F

    isEven (Int x) = liftBool $ even x
    isEven _ = F

    isOdd (Int x) = liftBool $ odd x
    isOdd _ = F

    isInteger (Int _) = T
    isInteger _ = F

instance Enum Number where
    toEnum = Int . toInteger
    fromEnum (Int x) = fromInteger x
    fromEnum _ = error "fromEnum: Not an Int"

instance Integral Number where
    quotRem (Int a) (Int b) = bimap Int Int (quotRem a b)
    quotRem _ _ = error "quotRem: Not an Int"

    toInteger (Int x) = x
    toInteger _ = error "toInteger: Not an Int"

-- | Obtiene el numerador de un 'Number'.
numerator :: Number -> Integer
numerator = R.numerator . toRational 

-- | Obtiene el denominador de un 'Number'.
denominator :: Number -> Integer
denominator = R.denominator . toRational

-- | Convierte un 'Number' a un 'Double'.
fromNumber :: Number -> Double
fromNumber (Int x) = fromIntegral x
fromNumber (Fraction x) = fromRational x
fromNumber (Real x) = x