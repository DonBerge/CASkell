{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr

import Symplify

import Control.Applicative

newtype Fail a = Fail { unFail :: Maybe a } deriving (Ord, Eq)

instance Functor Fail where
    fmap f (Fail x) = Fail $ f <$> x

instance Applicative Fail where
    pure = Fail . Just
    Fail f <*> Fail x = Fail $ f <*> x

instance Monad Fail where
    return = pure
    
    Fail Nothing >>= _ = Fail Nothing
    Fail (Just x) >>= f = f x

instance MonadFail Fail where
    fail _ = Fail Nothing

instance Alternative Fail where
    empty = Fail Nothing
    Fail Nothing <|> y = y
    x <|> _ = x

instance Show x => Show (Fail x) where
    show (Fail Nothing) = "Undefined"
    show (Fail (Just x)) = show x

type Expr = Fail PExpr

instance Num Expr where
    fromInteger = return . Number . fromInteger
    p + q = do
              p' <- p
              q' <- q
              simplifySum [p',q']
    p * q = do
              p' <- p
              q' <- q
              simplifyProduct [p',q']

    negate p = p >>= simplifyProduct . (:[-1])
    
    abs x
        | true $ isNegative x = negate x
        | true $ isPositive x = x
        | otherwise = fail "abs is undefined for this expression"
    
    signum = undefined
        
instance Fractional Expr where
    fromRational = return . Number . fromRational

    recip p = p >>= (`simplifyPow` (-1))

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f x = Fail $ f <$> unFail x 

---

instance Floating Expr where
    pi = return Pi
    exp = makeFun Exp
    log = makeFun Log

    sin = makeFun Sin

    cos 0 = 1
    cos x = makeFun Cos x
    
    tan = makeFun Tan
    asin = makeFun Asin
    acos = makeFun Acos
    atan = makeFun Atan
    sinh = makeFun Sinh
    cosh = makeFun Cosh
    tanh = makeFun Tanh
    asinh = makeFun Asinh
    acosh = makeFun Acosh
    atanh = makeFun Atanh

    sqrt x = x ** 0.5

    p ** q =  do
                p' <- p
                q' <- q
                simplifyPow p' q'

sec :: Expr -> Expr
sec = makeFun Sec

csc :: Expr -> Expr
csc = makeFun Csc

cot :: Expr -> Expr
cot = makeFun Cot

---------

extractTriBool :: Fail TriBool -> TriBool
extractTriBool (Fail Nothing) = U
extractTriBool (Fail (Just x)) = x

instance Assumptions Expr where
    isNegative = extractTriBool . fmap isNegative
    isPositive = extractTriBool . fmap isPositive
    isZero = extractTriBool . fmap isZero
    isEven = extractTriBool . fmap isEven
    isOdd = extractTriBool . fmap isOdd
    isInteger = extractTriBool . fmap isInteger
                        



--------


-------

number :: Rational -> Expr
number = fromRational

symbol :: String -> Expr
symbol = pure . Symbol