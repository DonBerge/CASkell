{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wall #-}

module PExpr (
    PExpr(..),

    
    -- * Entorno de suposiciones
    AssumptionsEnviroment(..),
    emptyAssumptions,
    -- ** Setters
    setPositive,
    setNegative,
    setZero,
    setEven,
    setOdd,
    setInteger,

    module Assumptions,
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
    pattern Atan,
    pattern Sinh,
    pattern Cosh,
    pattern Tanh,
    pattern Asinh,
    pattern Acosh,
    pattern Atanh
) where

import Data.Number


import Assumptions

-- = Entorno de suposiciones

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

-- Las PExpre construyen a partir de un conjunto de simbolos y constantes numericas
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

instance Ord PExpr where
    -- Simbolos
    SymbolWithAssumptions a _ < x = Fun a [] < x
    x < SymbolWithAssumptions a _ = x < Fun a []

    -- Constantes
    Number a < Number b = a < b
    Number _ < _ = True

    -- Productos
    Mul _ < Number _ = False
    Mul a < Mul b = reverse a < reverse b
    u@(Mul _) < v = u < Mul [v]


    -- Potencias
    Pow _ _ < Number _ = False
    u@(Pow _ _) < v@(Mul _) = v>=u
    Pow a b < Pow c d = if a/=c
                            then a < c
                            else b < d
    u@(Pow _ _) < v = u < Pow v (Number 1)

    -- Sumas
    Add _ < Number _ = False
    u@(Add _) < v@(Mul _) = v>=u
    u@(Add _) < v@(Pow _ _) = v>=u
    Add u < Add v = reverse u < reverse v
    u@(Add _) < v = u < Add [v]

    -- Funciones
    Fun f xs < Fun g ys
        | f /= g = f < g
        | otherwise = xs < ys

    u@(Fun _ _) < v = v>=u

    v >= u = not (v < u)

    u <= v = u<v || u==v

instance Assumptions PExpr where
    isZero (SymbolWithAssumptions _ a) = askZero a
    isZero u = liftBool $ u==(Number 0)


    isNegative (Number x) = isNegative x
    isNegative (SymbolWithAssumptions _ a) = askNegative a
    isNegative (Mul xs) = foldl1 xor $ map isNegative xs
        where
            xor U _ = U
            xor _ U = U
            xor p q = liftBool $ p /= q
    isNegative (Add xs) = foldTri uand T isNegative xs
        where
            -- la diferencia con (&&&) es que _ &&& F = F
            uand U _ = U
            uand _ U = U
            uand p q = p &&& q
    isNegative (Pow x y) = isNegative x &&& isOdd y
    isNegative (Exp _) = F
    isNegative (Log (Number a)) = liftBool $ a < 1
    isNegative Pi = F
    isNegative (Fun _ _) = U

    -- isZero (Number x) = isZero x
    -- isZero (SymbolWithAssumptions _ a) = askZero a
    -- isZero (Mul xs) = or3 isZero xs
    -- isZero (Add xs) = and3 isZero xs
    -- isZero (Pow x y) = isZero x &&& isPositive y
    -- isZero (Exp _) = F
    -- isZero (Log (Number a)) = liftBool $ a == 1
    -- isZero Pi = F
    -- isZero (Fun _ _) = U

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

pattern Symbol :: String -> PExpr
pattern Symbol x <- SymbolWithAssumptions x _

pattern Exp :: PExpr -> PExpr
pattern Exp x = Fun "exp" [x]

pattern Log :: PExpr -> PExpr
pattern Log x = Fun "log" [x]

pattern Sin :: PExpr -> PExpr
pattern Sin x = Fun "sin" [x]

pattern Cos :: PExpr -> PExpr
pattern Cos x = Fun "cos" [x]

pattern Asin :: PExpr -> PExpr
pattern Asin x = Fun "asin" [x]

pattern Acos :: PExpr -> PExpr
pattern Acos x = Fun "acos" [x]

pattern Atan :: PExpr -> PExpr
pattern Atan x = Fun "atan" [x]

pattern Sinh :: PExpr -> PExpr
pattern Sinh x = Fun "sinh" [x]

pattern Cosh :: PExpr -> PExpr
pattern Cosh x = Fun "cosh" [x]

pattern Tanh :: PExpr -> PExpr
pattern Tanh x = Fun "tanh" [x]

pattern Asinh :: PExpr -> PExpr
pattern Asinh x = Fun "asinh" [x]

pattern Acosh :: PExpr -> PExpr
pattern Acosh x = Fun "acosh" [x]

pattern Atanh :: PExpr -> PExpr
pattern Atanh x = Fun "atanh" [x]

pattern Pi :: PExpr
pattern Pi = SymbolWithAssumptions "pi" (AssumptionsEnviroment {
    askPositive = T,
    askNegative = F,
    askZero = F,
    askEven = F,
    askOdd = F,
    askInteger = F
})