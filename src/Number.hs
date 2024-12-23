{-# OPTIONS_GHC -Wall #-}

module Number (
    Number(..),
    numerator,
    denominator,
    isInteger
) where

import Data.Ratio ((%))
import qualified Data.Ratio as R

data Number = Int Integer | Fraction Rational | Real Double

digitCount :: Integer -> Int
digitCount = length . show . abs

epsilon :: Double
epsilon = 2**(-52)

maxDigits :: Int
maxDigits = 3

-- Realiza transformaciones entre los distintos tipos de números si es necesario
simplify :: Number -> Number
simplify (Fraction x) 
    | R.denominator x == 1 = Int (fromIntegral (R.numerator x))
    | dn <= maxDigits && dd <= maxDigits = Fraction x
    | otherwise = Real (fromRational x)
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

toFraction :: Number -> Number
toFraction (Int n) = Fraction (n%1)
toFraction (Real n) = Fraction (R.approxRational n epsilon)
toFraction x = x

toReal :: Number -> Number
toReal (Int n) = Real (fromIntegral n)
toReal (Fraction n) = Real (fromRational n)
toReal x = x


makeOp :: (Number -> Number -> t) -> Number -> Number -> t
makeOp op a@(Int _) b@(Int _) = a `op` b
makeOp op a@(Int _) b@(Fraction _) = toFraction a `op` b
makeOp op a@(Int _) b@(Real _) = toReal a `op` b
makeOp op a@(Fraction _) b@(Int _) = a `op` toFraction b
makeOp op a@(Fraction _) b@(Fraction _) = a `op` b
makeOp op a@(Fraction _) b@(Real _) = toReal a `op` b
makeOp op a@(Real _) b = a `op` toReal b

instance Eq Number where
  Int a == Int b = a==b
  Fraction a == Fraction b = a==b
  Real a == Real b = a==b

  a==b = makeOp (==) a b

instance Ord Number where
    Int a <= Int b = a<=b
    Fraction a <= Fraction b = a<=b
    Real a <= Real b = a<=b
    
    a<=b = makeOp (<=) a b

instance Show Number where
  show (Int n) = show n
  show (Fraction n) = show (R.numerator n) ++ "/" ++ show (R.denominator n)
  show (Real n) = show n


----

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

instance Fractional Number where
    fromRational = simplify . Fraction

    recip (Int a) = simplify $ Fraction (1%a)
    recip (Fraction a) = simplify $ Fraction $ recip a
    recip (Real a) = simplify $ Real $ recip a

makeFun :: (Double -> Double) -> Number -> Number
makeFun f (Real x) = simplify $ Real $ f x
makeFun f x = makeFun f $ toReal x

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

instance Real Number where
    toRational (Fraction a) = a
    toRational x = toRational $ toFraction x

numerator :: Number -> Integer
numerator = R.numerator . toRational 

denominator :: Number -> Integer
denominator = R.denominator . toRational

isInteger :: Number -> Bool
isInteger x = case simplify x of
                Int _ -> True
                _ -> False