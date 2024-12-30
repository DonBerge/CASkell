{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr
import Symplify

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


type Expr = Fail PExpr


pattern Symbol :: String -> PExpr
pattern Symbol x = Fun x []

pattern Exp :: PExpr -> PExpr
pattern Exp x = Fun "Exp" [x]

pattern Log :: PExpr -> PExpr
pattern Log x = Fun "Log" [x]

pattern Sin :: PExpr -> PExpr
pattern Sin x = Fun "Sin" [x]

pattern Cos :: PExpr -> PExpr
pattern Cos x = Fun "Cos" [x]

pattern Tan :: PExpr -> PExpr
pattern Tan x = Fun "Tan" [x]

pattern Cot :: PExpr -> PExpr
pattern Cot x = Fun "Cot" [x]

pattern Sec :: PExpr -> PExpr
pattern Sec x = Fun "Sec" [x]

pattern Csc :: PExpr -> PExpr
pattern Csc x = Fun "Csc" [x]

pattern Asin :: PExpr -> PExpr
pattern Asin x = Fun "Asin" [x]

pattern Acos :: PExpr -> PExpr
pattern Acos x = Fun "Acos" [x]

pattern Atan :: PExpr -> PExpr
pattern Atan x = Fun "Atan" [x]

pattern Sinh :: PExpr -> PExpr
pattern Sinh x = Fun "Sinh" [x]

pattern Cosh :: PExpr -> PExpr
pattern Cosh x = Fun "Cosh" [x]

pattern Tanh :: PExpr -> PExpr
pattern Tanh x = Fun "Tanh" [x]

pattern Asinh :: PExpr -> PExpr
pattern Asinh x = Fun "Asinh" [x]

pattern Acosh :: PExpr -> PExpr
pattern Acosh x = Fun "Acosh" [x]

pattern Atanh :: PExpr -> PExpr
pattern Atanh x = Fun "Atanh" [x]

pattern Pi :: PExpr
pattern Pi = Symbol "Pi"

instance Num Expr where
    fromInteger = return . Number . fromInteger
    p + q = do
              p' <- p
              q' <- q
              simplifySum [p', q']
    p * q = do
              p' <- p
              q' <- q
              simplifyProduct [p', q']

    negate p = p >>= simplifyProduct . (:[Number (-1)])

    p - q = do
              p' <- p
              q' <- negate q
              simplifySum [p', q']

    abs = undefined
    signum = undefined

instance Fractional Expr where
    fromRational = return . Number . fromRational
    p / q = do
              p' <- p
              q' <- q >>= (`simplifyPow` (-1))
              simplifyProduct [p', q']

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f x = Fail $ f <$> unFail x 

instance Floating Expr where
    pi = return Pi
    exp = makeFun Exp
    log = makeFun Log
    sin = makeFun Sin
    cos = makeFun Cos
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

    p ** q =  do
                p' <- p
                q' <- q
                simplifyPow p' q'


cot :: Expr -> Expr
cot = makeFun Cot

-------

instance Show Expr where
    show (Fail Nothing) = "Undefined"
    show (Fail (Just x)) = show x

number :: Rational -> Expr
number = fromRational

symbol :: String -> Expr
symbol = pure . Symbol