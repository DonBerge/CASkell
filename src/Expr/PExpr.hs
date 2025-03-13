{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wall #-}

{-|
    Module      : Expr.PExpr
    Description : Definición de arboles de expresiones matemáticas puras.
-}
module PExpr (
    PExpr(..),

    
    -- * Entorno de suposiciones
    module Assumptions,
    AssumptionsEnviroment(..),
    emptyAssumptions,
    -- ** Setters
    setPositive,
    setNegative,
    setZero,
    setEven,
    setOdd,
    setInteger,
    -- * Patrones
    -- ** Simbolos
    pattern Pi,
    pattern Symbol,
    -- ** Funciones
    pattern Exp,
    pattern Log,
    pattern Sin,
    pattern Cos,
    pattern Asin,
    pattern Acos,
    pattern Atan
) where

import Data.Number


import Assumptions

-- | Entorno de suposiciones, contiene las suposiciones que se han hecho sobre un cierto tipo de datos
data AssumptionsEnviroment = AssumptionsEnviroment {
    askPositive :: TriBool,
    askNegative :: TriBool,
    askZero :: TriBool,
    askEven :: TriBool,
    askOdd :: TriBool,
    askInteger :: TriBool
} deriving (Show)

-- | Entorno de suposiciones con todas las suposiciones desconocidas
emptyAssumptions :: AssumptionsEnviroment
emptyAssumptions = AssumptionsEnviroment U U U U U U

setPositive :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setPositive p env = env { askPositive = p }

setNegative :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setNegative n env = env { askNegative = n }

setZero :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setZero z env = env { askZero = z }

setEven :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setEven e env = env { askEven = e }

setOdd :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setOdd o env = env { askOdd = o }

setInteger :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setInteger i env = env { askInteger = i }

-- | Las PExpre construyen a partir de un conjunto de simbolos y constantes numericas
data PExpr = Number Number 
                | SymbolWithAssumptions String AssumptionsEnviroment
                | Mul [PExpr] 
                | Add [PExpr] 
                | Pow PExpr PExpr
                | Fun String [PExpr]
             deriving Show

instance Eq PExpr where
    (Number a) == (Number b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (Mul a) == (Mul b) = a == b
    (Add a) == (Add b) = a == b
    (Pow a b) == (Pow c d) = a == c && b == d
    (Fun a b) == (Fun c d) = a == c && b == d
    _ == _ = False

-- | Orden de las expresiones
instance Ord PExpr where
    -- Constantes
    -- Los numeros se ordenan de menor a mayor y aparecen antes que cualquier otra expresion
    Number a < Number b = a < b
    Number _ < _ = True

    -- Simbolos
    -- Los simbolos se comparan igual que las funciones, solo que son tratados como funciones sin argumentos
    SymbolWithAssumptions a _ < x = Fun a [] < x
    x < SymbolWithAssumptions a _ = x < Fun a []

    -- Productos
    -- Los productos se comparan por el orden lexicografico inverso de sus factores
    -- Para comparar una expresion que no es un producto con otra que si lo es, se la toma como un producto de un solo factor
    Mul _ < Number _ = False
    Mul a < Mul b = reverse a < reverse b
    u@(Mul _) < v = u < Mul [v]


    -- Potencias
    Pow _ _ < Number _ = False      
    u@(Pow _ _) < v@(Mul _) = v>=u -- Usar las reglas de comparacion de productos
    Pow a b < Pow c d = if a/=c    -- Si las bases son distintas, se comparan las bases, sino los exponentes
                            then a < c
                            else b < d
    u@(Pow _ _) < v = u < Pow v (Number 1) -- Al comparar con expresiones v que no son potencias, se toma v como v^1 y se compara usando
                                           -- las reglas de potencias

    -- Sumas
    Add _ < Number _ = False
    u@(Add _) < v@(Mul _) = v>=u           -- Usar las comparaciones de productos
    u@(Add _) < v@(Pow _ _) = v>=u         -- Usar las comparaciones de potencias
    Add u < Add v = reverse u < reverse v  -- Sino comparar por el orden lexicografico inverso
    u@(Add _) < v = u < Add [v]            -- Tratar v como una suma unaria

    -- Funciones
    Fun f xs < Fun g ys                    -- Comparar por orden lexicografico de los nombres o por orden
        | f /= g = f < g                   -- lexicografico de los argumentos si los nombres son iguales
        | otherwise = xs < ys

    u@(Fun _ _) < v = v>=u                 -- Usar las reglas de comparacion anteriores

    v >= u = not (v < u)

    u <= v = u<v || u==v

instance Assumptions PExpr where
    isZero (SymbolWithAssumptions _ a) = askZero a
    isZero u = liftBool $ u==(Number 0)

    isNegative (Number x) = isNegative x
    isNegative (SymbolWithAssumptions _ a) = askNegative a
    isNegative (Mul xs) = xor3 isNegative xs
    isNegative (Add xs)
        | all (true . isNegative) xs = T
        | all (false . isNegative) xs = F
        | otherwise = U
    isNegative (Pow x y) = isNegative x &&& isOdd y
    isNegative (Exp _) = F
    isNegative (Log (Number a)) = liftBool $ a < 1
    isNegative Pi = F
    isNegative (Fun _ _) = U

    isInteger (Number x) = isInteger x
    isInteger (SymbolWithAssumptions _ a) = askInteger a
    isInteger (Mul xs) = and3 isInteger xs
    isInteger (Add xs) = and3 isInteger xs
    isInteger (Pow x y) = isInteger x &&& isInteger y
    isInteger Pi = F
    isInteger (Fun _ _) = U

    isEven (Number x) = isEven x
    isEven (SymbolWithAssumptions _ a) = askEven a
    isEven (Mul xs) = and3 isInteger xs &&& or3 isEven xs
    isEven (Add xs) = and3 isInteger xs &&& xor3 isOdd xs
    isEven (Pow x y) = isEven x &&& isInteger y
    isEven Pi = F
    isEven _ = U
    
    isOdd = not3 . isEven

-- | Patron para simbolos que ignora las suposiciones
pattern Symbol :: String -> PExpr
pattern Symbol x <- SymbolWithAssumptions x _

-- | Patron para la funcion exponencial
pattern Exp :: PExpr -> PExpr
pattern Exp x = Fun "exp" [x]

-- | Patron para la funcion logaritmo
pattern Log :: PExpr -> PExpr
pattern Log x = Fun "log" [x]

-- | Patron para la funcion seno
pattern Sin :: PExpr -> PExpr
pattern Sin x = Fun "sin" [x]

-- | Patron para la funcion coseno
pattern Cos :: PExpr -> PExpr
pattern Cos x = Fun "cos" [x]

-- | Patron para la funcion arcoseno
pattern Asin :: PExpr -> PExpr
pattern Asin x = Fun "asin" [x]

-- | Patron para la funcion arcocoseno
pattern Acos :: PExpr -> PExpr
pattern Acos x = Fun "acos" [x]

-- | Patron para la funcion arcotangente
pattern Atan :: PExpr -> PExpr
pattern Atan x = Fun "atan" [x]

-- | Patron para el simbolo pi con suposiciones incluidas
pattern Pi :: PExpr
pattern Pi = SymbolWithAssumptions "pi" (AssumptionsEnviroment {
    askPositive = T,
    askNegative = F,
    askZero = F,
    askEven = F,
    askOdd = F,
    askInteger = F
})