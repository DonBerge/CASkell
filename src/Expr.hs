{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr

import Symplify

import Data.List
import Number (Number)
import qualified Number as N

-- import Simplification.Rationalize

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

    p / q = do
              p' <- p
              q' <- q
              simplifyDiv p' q'
              -- rationalize pdivq

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

number :: Number -> Expr
number = return . Number

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

{-
numerator :: Expr -> Expr
numerator (structure -> Number n) = fromInteger $ N.numerator n
numerator (structure -> Mul xs) = product $ fmap numerator xs
numerator (structure -> Pow _ y)
    | true $ isNegative y = 1
numerator (structure -> Exp x)
    | true $ isNegative x = 1
numerator x = x

denominator :: Expr -> Expr
denominator (structure -> Number n) = fromInteger $ N.denominator n
denominator (structure -> Mul xs) = product $ fmap denominator xs
denominator u@(structure -> Pow _ y)
    | true $ isNegative y = recip u
denominator (structure -> Exp x)
    | true $ isNegative x = exp (-x)
denominator _ = 1
-}

numerator :: Expr -> Expr
numerator = (=<<) numerator'
    where
        numerator' (Number n) = fromInteger $ N.numerator n
        numerator' (Mul xs) = mapM numerator' xs >>= simplifyProduct --product $ fmap numerator' xs
        numerator' (Pow _ y)
            | true $ isNegative y = return 1
        numerator' (Exp x)
            | true $ isNegative x = return 1
        numerator' x = return x

denominator :: Expr -> Expr
denominator = (=<<) denominator'
    where
        denominator' (Number n) = fromInteger $ N.denominator n
        denominator' (Mul xs) = mapM denominator' xs >>= simplifyProduct --product $ fmap denominator' xs
        denominator' u@(Pow _ y)
            | true $ isNegative y = simplifyDiv 1 u
        denominator' (Exp x)
            | true $ isNegative x = simplifyNegate x >>= simplifyFun . Exp
        denominator' _ = return 1