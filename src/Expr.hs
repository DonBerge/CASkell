{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr
import Symplify

type Expr' = PExpr String

newtype Expr = Expr { unExpr :: Maybe Expr' }

pattern Symbol :: String -> Expr'
pattern Symbol x = Fun x []

pattern Exp :: Expr' -> Expr'
pattern Exp x = Fun "Exp" [x]

pattern Log :: Expr' -> Expr'
pattern Log x = Fun "Log" [x]

pattern Sin :: Expr' -> Expr'
pattern Sin x = Fun "Sin" [x]

pattern Cos :: Expr' -> Expr'
pattern Cos x = Fun "Cos" [x]

pattern Tan :: Expr' -> Expr'
pattern Tan x = Fun "Tan" [x]

pattern Cot :: Expr' -> Expr'
pattern Cot x = Fun "Cot" [x]

pattern Sec :: Expr' -> Expr'
pattern Sec x = Fun "Sec" [x]

pattern Csc :: Expr' -> Expr'
pattern Csc x = Fun "Csc" [x]

pattern Asin :: Expr' -> Expr'
pattern Asin x = Fun "Asin" [x]

pattern Acos :: Expr' -> Expr'
pattern Acos x = Fun "Acos" [x]

pattern Atan :: Expr' -> Expr'
pattern Atan x = Fun "Atan" [x]

pattern Sinh :: Expr' -> Expr'
pattern Sinh x = Fun "Sinh" [x]

pattern Cosh :: Expr' -> Expr'
pattern Cosh x = Fun "Cosh" [x]

pattern Tanh :: Expr' -> Expr'
pattern Tanh x = Fun "Tanh" [x]

pattern Asinh :: Expr' -> Expr'
pattern Asinh x = Fun "Asinh" [x]

pattern Acosh :: Expr' -> Expr'
pattern Acosh x = Fun "Acosh" [x]

pattern Atanh :: Expr' -> Expr'
pattern Atanh x = Fun "Atanh" [x]

pattern Pi :: Expr'
pattern Pi = Symbol "Pi"

instance Num Expr where
    fromInteger = Expr . Just . Number . fromInteger
    p + q = Expr $ do
                    p' <- unExpr p
                    q' <- unExpr q
                    simplifySum [p', q']
    p * q = Expr $ do
                    p' <- unExpr p
                    q' <- unExpr q
                    simplifyProduct [p', q']

    negate p = Expr $ unExpr p >>= simplifyProduct . (:[Number (-1)])

    p - q = Expr $ do
                    p' <- unExpr p
                    q' <- unExpr $ negate q
                    simplifySum [p', q']

    abs = undefined
    signum = undefined

instance Fractional Expr where
    fromRational = Expr . Just . Number . fromRational
    p / q = Expr $ do
                    p' <- unExpr p
                    q' <- (unExpr q) >>= (`simplifyPow` (-1))
                    simplifyProduct [p', q']

makeFun f x = Expr $ f <$> unExpr x 

instance Floating Expr where
    pi = Expr $ Just Pi
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


    p ** q =  Expr $ do
                        p' <- unExpr p
                        q' <- unExpr q
                        simplifyPow p' q'

-------


-- number :: Rational -> Expr
-- number = fromRational
-- 
-- symbol :: String -> Expr
-- symbol = return . Symbol