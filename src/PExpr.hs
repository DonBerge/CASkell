{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module PExpr (
    PExpr(..),
    eMap,
    showStruct,
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

-- Las PExpre construyen a partir de un conjunto de simbolos y constantes numericas
data PExpr = Number Number 
                | Mul [PExpr] 
                | Add [PExpr] 
                | Pow PExpr PExpr
                | Fun String [PExpr]

instance Eq PExpr where
    (Number a) == (Number b) = a == b
    (Mul a) == (Mul b) = a == b
    (Add a) == (Add b) = a == b
    (Pow a b) == (Pow c d) = a == c && b == d
    (Fun a b) == (Fun c d) = a == c && b == d
    _ == _ = False

instance Ord PExpr where

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

unquote :: String -> String
unquote ('"':s) | last s == '"' = init s
unquote s = s

paren :: String -> String
paren s = "(" ++ s ++ ")"

isNegative :: PExpr -> Bool
isNegative (Number x) = x < 0
isNegative (Mul xs) = all isNegative xs
isNegative _ = False

-- TODO: usar DOC
instance Show PExpr where
    show (Number x) = show x
    -- show (Symbol x) = unquote $ show x
    show (Mul []) = "1"
    show (Mul xs) = intercalate "*" $ map parenExpr xs
        where
            parenExpr (Number s)
                | s < 0 || not (isInteger s) = paren $ show s
                | otherwise = show s
            parenExpr s@(Add _) = paren $ show s
            parenExpr s = show s
    show (Add []) = "0"
    show (Add xs) = foldl (\x y -> case x of
                                    "" -> show y
                                    _ | mulByNeg y -> x ++ "-" ++ show (negate y)
                                    _ -> x ++ "+" ++ parenExpr y
                            ) "" xs --intercalate "+" $ map parenExpr xs
        where
            mulByNeg (Mul ((Number a):_)) = a < 0
            mulByNeg _ = False

            parenExpr (Number s)
                | s < 0 || not (isInteger s) = paren $ show s
                | otherwise = show s
            parenExpr s@(Add _) = paren $ show s
            parenExpr s = show s
    
    show (Pow x (Number a))
        | a == 1/2 = "√" ++ "(" ++ show x ++ ")"
        | a == 1/3 = "∛" ++ "(" ++ show x ++ ")"
        | a == 1/4 = "∜" ++ "(" ++ show x ++ ")"
     -- = parenExpr x ++ "^2"
    show (Pow x y)
        | y == 1 = show x
        | isNegative y = "1/" ++ parenExpr (Pow x (negate y))
        | otherwise = parenExpr x ++ "^" ++ parenExpr y
        where
            parenExpr (Number s)
                | s < 0 || not (isInteger s) = paren $ show s
                | otherwise = show s
            parenExpr s@(Add _) = "(" ++ show s ++ ")"
            parenExpr s@(Mul _) = "(" ++ show s ++ ")"
            parenExpr s@(Pow _ _) = "(" ++ show s ++ ")"
            parenExpr s = show s
    show (Fun f []) = unquote $ show f
    show (Fun f xs) = unquote (show f) ++ "(" ++ intercalate "," (map show xs) ++ ")"


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
        | otherwise = Mul $ (Number (negate a)):ps
    negate (Mul ps) = Mul ((fromInteger (-1)):ps)
    negate e = Mul [fromInteger (-1), e]

    abs = undefined
    signum = undefined

eMap :: (PExpr -> PExpr) -> PExpr -> PExpr
eMap _ (Number x) = Number x
eMap f (Mul xs) = Mul (map f xs)
eMap f (Add xs) = Add (map f xs)
eMap f (Pow x y) = Pow (f x) (f y)
eMap f (Fun s xs) = Fun s $ map f xs

showStruct :: PExpr -> String
showStruct (Number x) = "Number " ++ show x
showStruct (Mul xs) = "Mul [" ++ intercalate "," (map showStruct xs) ++ "]"
showStruct (Add xs) = "Add [" ++ intercalate "," (map showStruct xs) ++ "]"
showStruct (Pow x y) = "Pow (" ++ showStruct x ++ ") (" ++ showStruct y ++ ")"
showStruct (Fun s xs) = "Fun " ++ show s ++ " [" ++ intercalate "," (map showStruct xs) ++ "]"

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