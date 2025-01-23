{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr

import Symplify

import Data.List
import Classes.EvalSteps

type Expr = EvalSteps PExpr

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
    
    abs x = do
              x' <- x
              case x' of
                Number a -> return $ Number $ abs a
                _ -> sqrt (x ** 2)
    
    signum 0 = 0
    signum x = x / abs x
        
instance Fractional Expr where
    fromRational = return . Number . fromRational

    recip p = p >>= (`simplifyPow` (-1))

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f = (=<<) (simplifyFun . f)

---

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

extractTriBool :: EvalSteps TriBool -> TriBool
extractTriBool (EvalSteps (Left _, _)) = U
extractTriBool (EvalSteps (Right x, _)) = x

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

undefinedExpr :: Expr
undefinedExpr = fail "Explicit undefined"

--

-- | Muestra la estructura interna de la expresion, util para debuggear
showStruct :: Expr -> String
showStruct (EvalSteps (Left e, _)) = "Undefined: " ++ e 
showStruct (EvalSteps (Right e, _)) = showStruct' e
    where
        unquote :: String -> String
        unquote [] = []
        unquote [x] = [x]
        unquote (x:xs) = if x == last xs then init xs else x:xs

        showStruct' (Number n) = "Number " ++ show n
        showStruct' (Symbol x) = unquote x
        showStruct' (Mul xs) = "Mul ( (" ++ intercalate ") , (" (map showStruct' xs) ++ ") )"
        showStruct' (Add xs) = "Add ( (" ++ intercalate ") , (" (map showStruct' xs) ++ ") )"
        showStruct' (Pow x y) = "Pow (" ++ showStruct' x ++ "), (" ++ showStruct' y ++ ")"
        showStruct' (Fun f xs) ="Fun " ++ unquote f ++ " " ++ intercalate "," (map showStruct' xs)