{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PExpr (
    PExpr(..),
    module TriBool,
    module Classes.Assumptions,
    pattern Pi,
    pattern Symbol,
    pattern Exp,
    pattern Log,
    pattern Sin,
    pattern Cos,
    pattern Tan,
    pattern Cot,
    pattern Sec,
    pattern Csc,
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

import Number

import Data.List

import Classes.Assumptions
import TriBool
import Classes.Monads.MonadAssumptions

-- Las PExpre construyen a partir de un conjunto de simbolos y constantes numericas
data PExpr = Number Number 
                | SymbolWithAssumptions String AssumptionsEnviroment
                | Mul [PExpr] 
                | Add [PExpr] 
                | Pow PExpr PExpr
                | Fun String [PExpr]

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

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- isNegative :: PExpr -> Bool
-- isNegative (Number x) = x < 0
-- isNegative _ = False

-- TODO: usar DOC
instance Show PExpr where
    show (Number x) = show x
    show (Symbol x) = x
    
    show (Mul []) = "1"
    show (Mul xs) = intercalate "*" $ map parenExpr xs
        where
            parenExpr s@(Add _) = paren $ show s
            parenExpr s = show s
    show (Add []) = "0"
    show (Add xs) = intercalate "+" $ map parenExpr xs
        where
            parenExpr s@(Add _) = paren $ show s
            parenExpr s = show s
    
    show (Pow x y) = parenExpr x ++ "^" ++ parenExpr y
        where
            parenExpr (Number s)
                | true (s >= 0 &&& isInteger s) = show s
                | otherwise = paren $ show s
            parenExpr s@(Add _) = "(" ++ show s ++ ")"
            parenExpr s@(Mul _) = "(" ++ show s ++ ")"
            parenExpr s@(Pow _ _) = "(" ++ show s ++ ")"
            parenExpr s = show s
    show (Fun f []) = f
    show (Fun f xs) = f ++ "(" ++ intercalate "," (map show xs) ++ ")"

instance Assumptions PExpr where
    {-isPositive (Number x) = isPositive x
    isPositive (SymbolWithAssumptions _ a) = askPositive a
    isPositive (Mul xs) = xor3 isNegative xs
    isPositive (Add []) = F
    isPositive (Add xs) = foldTri uand T isPositive xs
        where
            uand U _ = U
            uand _ U = U
            uand p q = p &&& q
    isPositive (Pow x y) = isPositive x ||| isEven y
    isPositive (Exp _) = T
    isPositive (Log (Number a)) = liftBool $ a > 1
    isPositive Pi = T
    isPositive (Fun _ _) = U-}
    isPositive x = not3 $ isNegative x ||| isZero x
    

    isNegative (Number x) = isNegative x
    isNegative (SymbolWithAssumptions _ a) = askNegative a
    isNegative (Mul xs) = foldl nxor T $ map isNegative xs
        where
            nxor U _ = U
            nxor _ U = U
            nxor p q = liftBool $ p == q
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

    isZero (Number x) = isZero x
    isZero (SymbolWithAssumptions _ a) = askZero a
    isZero (Mul xs) = or3 isZero xs
    isZero (Add xs) = and3 isZero xs
    isZero (Pow x y) = isZero x &&& isPositive y
    isZero (Exp _) = F
    isZero (Log (Number a)) = liftBool $ a == 1
    isZero Pi = F
    isZero (Fun _ _) = U

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


instance Num PExpr where
    fromInteger x = Number (fromInteger x)
    (Add ps) + (Add qs) = Add $ ps ++ qs
    p + (Add qs) = Add $ p:qs
    (Add ps) + q = Add $ ps ++ [q]
    p + q = Add [p, q]

    (Mul ps) * (Mul qs) = Mul $ ps ++ qs
    p * (Mul qs) = Mul $ p:qs
    (Mul ps) * q = Mul $ ps ++ [q]
    p * q = Mul [p, q]

    negate (Number x) = Number (negate x)
    negate (Add ps) = Add $ map negate ps
    negate (Mul ((Number a): ps))
        | a == -1 = Mul ps
        | otherwise = Mul $ Number (negate a):ps
    negate (Mul ps) = Mul (fromInteger (-1):ps)
    negate e = Mul [fromInteger (-1), e]

    abs x = Pow (Pow x 2) (Number 0.5)
    signum 0 = 0
    signum x = abs x / x

instance Fractional PExpr where
    fromRational x = Number (fromRational x)
    recip (Number x) = Number (recip x)
    recip x = Pow x (-1)

pattern Symbol :: String -> PExpr
pattern Symbol x <- SymbolWithAssumptions x _

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
pattern Pi = SymbolWithAssumptions "Pi" (AssumptionsEnviroment {
    askPositive = T,
    askNegative = F,
    askZero = F,
    askEven = F,
    askOdd = F,
    askInteger = F
})