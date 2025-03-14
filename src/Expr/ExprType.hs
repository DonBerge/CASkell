{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
    Module      : Expr.ExprType
    Description : Módulo que define el tipo 'Expr' y sus operaciones.
    
-}
module Expr.ExprType (
    -- * El tipo 'Expr'
    Expr,
    sec,
    csc,
    cot,
    -- * Conversiones
    fromNumber,
    function,
    symbol,
    symbols,
    assume,
    undefinedExpr,
    -- * Operadores
    -- ** Operadores racionales
    numerator,
    denominator,
    -- ** Operadores de comparación
    lte,
    lt,
    gt,
    gte,
    -- ** Muestreo de Expr
    showTree
) where

import Prelude hiding (const, exponent)

import Expr.Simplify

import Data.Number (Number)
import qualified Data.Number as Number
import Data.Either (fromRight)

-- import Simplification.Rationalize

type Expr = EvalResult PExpr

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

    negate p = p >>= simplifyProduct . (:[Number (-1)])

    abs x = do
              x' <- x
              case x' of
                Number a -> return $ Number $ abs a
                _ -> sqrt (x ** 2)

    signum 0 = 0
    signum x = x / abs x

instance Fractional Expr where
    fromRational = return . Number . fromRational

    recip p = p >>= (`simplifyPow` Number (-1))

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f = (=<<) (simplifyFun . f)

---

instance Floating Expr where
    pi = return Pi
    exp = makeFun Exp
    log = makeFun Log
    sin = makeFun Sin
    cos = makeFun Cos
    tan x = sin x / cos x
    asin = makeFun Asin
    acos = makeFun Acos
    atan = makeFun Atan

    -- Las funciones hiperbolicas se definen en terminos de la exponencial
    sinh x = exp x / 2 - exp(-x) / 2
    cosh x = exp x / 2 + exp(-x) / 2
    tanh x = (exp x - exp (-x)) / (exp x + exp (-x))

    -- Las funciones inversas hiperbolicas se definen en terminos del logaritmo
    asinh x = log(x + sqrt (x ** 2 + 1))
    acosh x = log(x + sqrt (x ** 2 - 1))
    atanh x = log(1+x)/2 - log(1-x)/2
    sqrt x = x ** 0.5

    p ** q =  do
                p' <- p
                q' <- q
                simplifyPow p' q'

sec :: Expr -> Expr
sec = recip . cos

csc :: Expr -> Expr
csc = recip . sin

cot :: Expr -> Expr
cot = recip . tan

-- * Assumptions sobre las expresiones

extractTriBool :: EvalResult TriBool -> TriBool
extractTriBool = fromRight U . runEvalResult

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

-------

fromNumber :: Number -> Expr
fromNumber = return . Number

function :: String -> [Expr] -> Expr
function f [] = symbol f
function f xs = sequence xs >>= simplifyFun . Fun f

symbol :: String -> Expr
symbol = return . flip SymbolWithAssumptions emptyAssumptions

symbols :: String -> [Expr]
symbols = map symbol . words

assume :: Expr -> [String] -> Expr
assume = foldl (\x y -> x >>= assume' y)
    where
        assume' "zero" (SymbolWithAssumptions _ _) = return $ Number 0
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
undefinedExpr = fail

numerator :: Expr -> Expr
numerator = (=<<) numerator'
    where
        -- Multiplicación rapida de PExpr, ya que no es necesaria simplificación
        numerator' (Number n)
            | Number.printAsFraction n = fromInteger $ Number.numerator n
            | otherwise = return (Number n)
        numerator' (Mul xs) = mapM numerator' xs >>= simplifyProduct
        numerator' (Pow _ y)
            | mulByNeg y = return (Number 1)
        --numerator' (Exp x)
        --    | mulByNeg x = return (Number 1)
        numerator' x = return x

denominator :: Expr -> Expr
denominator = (=<<) denominator'
    where
        denominator' (Number n)
            | Number.printAsFraction n = fromInteger $ Number.denominator n
            | otherwise = return (Number 1)
        denominator' (Mul xs) = mapM denominator' xs >>= simplifyProduct
        denominator' u@(Pow _ y)
            | mulByNeg y = simplifyDiv (Number 1) u
        --denominator' (Exp x)
        --    | mulByNeg x = simplifyNegate x >>= simplifyFun . Exp
        denominator' _ = return (Number 1)

showTree :: Expr -> String
showTree t = case runEvalResult t of
                Left e -> "Undefined: " ++ e
                Right e -> show e