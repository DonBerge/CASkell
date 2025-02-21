{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr

import Symplify

import Number (Number)
import qualified Number as N
import Control.Monad.Except (MonadError(throwError))
import Classes.Monads.MonadAssumptions (emptyAssumptions, setPositive, setNegative, setZero, setInteger, setOdd, setEven)
import Data.Either (fromRight)

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

-- * Assumptions sobre las expresiones

extractTriBool :: EvalSteps TriBool -> TriBool
extractTriBool = fromRight U . runEvalSteps

instance Assumptions Expr where
    isNegative = extractTriBool . fmap isNegative
    isPositive = extractTriBool . fmap isPositive
    isZero = extractTriBool . fmap isZero
    isEven = extractTriBool . fmap isEven
    isOdd = extractTriBool . fmap isOdd
    isInteger = extractTriBool . fmap isInteger

-- * Comparacion de expresiones              

lte :: Expr -> Expr -> TriBool
lte x y = isNegative (x-y) ||| x==y -- (x-y) negativo o x==y

lt :: Expr -> Expr -> TriBool
lt x y = isNegative (x-y) -- (y-x) negativo y x/=y

gt :: Expr -> Expr -> TriBool
gt x y = not3 $ lte x y

gte :: Expr -> Expr -> TriBool
gte x y = not3 $ lt x y

--------


-------

number :: Number -> Expr
number = return . Number

symbol :: String -> Expr
symbol = return . flip SymbolWithAssumptions emptyAssumptions

assume :: Expr -> [String] -> Expr
assume u a = foldl (\x y -> x >>= assume' y) u a
    where
        assume' b (SymbolWithAssumptions y env) = return $ SymbolWithAssumptions y (setAssumption b env)
        assume' _ x = return x

        setAssumption "positive" = setNegative F . setZero F . setPositive T
        setAssumption "negative" = setNegative T . setZero F . setPositive F
        setAssumption "zero" = setNegative F . setZero T . setPositive F
        setAssumption "integer" = setEven U . setOdd U . setInteger T
        setAssumption "even" = setEven T . setOdd F . setInteger T
        setAssumption "odd" = setOdd T . setEven F . setInteger T
        setAssumption _ = id




undefinedExpr :: String -> Expr
undefinedExpr = throwError

--

-- | Muestra la estructura interna de la expresion, util para debuggear
-- showStruct :: Expr -> String
-- showStruct (EvalSteps (Left e, _)) = "Undefined: " ++ e 
-- showStruct (EvalSteps (Right e, _)) = showStruct' e
--     where
--         unquote :: String -> String
--         unquote [] = []
--         unquote [x] = [x]
--         unquote (x:xs) = if x == last xs then init xs else x:xs
-- 
--         showStruct' (Number n) = "Number " ++ show n
--         showStruct' (Symbol x) = unquote x
--         showStruct' (Mul xs) = "Mul ( (" ++ intercalate ") , (" (map showStruct' xs) ++ ") )"
--         showStruct' (Add xs) = "Add ( (" ++ intercalate ") , (" (map showStruct' xs) ++ ") )"
--         showStruct' (Pow x y) = "Pow (" ++ showStruct' x ++ "), (" ++ showStruct' y ++ ")"
--         showStruct' (Fun f xs) ="Fun " ++ unquote f ++ " " ++ intercalate "," (map showStruct' xs)

numerator :: Expr -> Expr
numerator = (=<<) numerator'
    where
        -- Multiplicación rapida de PExpr, ya que no es necesaria simplificación
        numerator' (Number n) = fromInteger $ N.numerator n
        numerator' (Mul xs) = mapM numerator' xs >>= return . product
        numerator' (Pow _ y)
            | mulByNeg y = return 1
        numerator' (Exp x)
            | mulByNeg x = return 1
        numerator' x = return x

denominator :: Expr -> Expr
denominator = (=<<) denominator'
    where
        denominator' (Number n) = fromInteger $ N.denominator n
        denominator' (Mul xs) = mapM denominator' xs >>= return . product
        denominator' u@(Pow _ y)
            | mulByNeg y = simplifyDiv 1 u
        denominator' (Exp x)
            | mulByNeg x = simplifyNegate x >>= simplifyFun . Exp
        denominator' _ = return 1