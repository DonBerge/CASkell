{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr
import Symplify

import qualified Number as N

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


instance Show x => Show (Fail x) where
    show (Fail Nothing) = "Undefined"
    show (Fail (Just x)) = show x

type Expr = Fail PExpr

instance Num Expr where
    fromInteger = return . Number . fromInteger
    p + q = do
              p' <- p
              q' <- q
              Add xs <- return $ p' + q'
              simplifySum xs
    p * q = do
              p' <- p
              q' <- q
              Mul xs <- return $ p' * q'
              simplifyProduct xs

    negate p = p >>= simplifyProduct . (:[Number (-1)])

    p - q = do
              p' <- p
              q' <- negate q
              Add xs <- return $ p' + q'
              simplifySum xs

    abs = undefined
    signum = undefined

instance Fractional Expr where
    fromRational = return . Number . fromRational
    p / q = do
              p' <- p
              q' <- q >>= (`simplifyPow` (-1))
              Mul xs <- return $ p' * q'
              simplifyProduct xs

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f x = Fail $ f <$> unFail x 

instance Floating Expr where
    pi = return Pi
    exp = makeFun Exp
    log = makeFun Log

    sin 0 = 0
    sin x = makeFun Sin x

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

numerator :: PExpr -> Expr
numerator (Number n) = fromInteger $ N.numerator n
numerator (Add []) = numerator 0
numerator (Mul []) = numerator 1
numerator (Mul xs) = product $ map numerator xs
numerator (Pow _ y)
    | isTrue $ isNegative y = 1
numerator (Exp x)
    | isTrue $ isNegative x = 1
numerator x = return x    

denominator :: PExpr -> Expr
denominator (Number n) = fromInteger $ N.denominator n
denominator (Add []) = denominator 0
denominator (Mul []) = denominator 1
denominator (Mul xs) = product $ map denominator xs
denominator u@(Pow _ y)
    | isTrue $ isNegative y = recip $ return u
denominator (Exp x)
    | isTrue $ isNegative x = exp $ negate $ return x
denominator _ = 1


-------

number :: Rational -> Expr
number = fromRational

symbol :: String -> Expr
symbol = pure . Symbol