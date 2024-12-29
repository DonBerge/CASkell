{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module PExpr (
    PExpr(..),
) where

import Number

import Control.Monad

import Data.List

-- Las PExpr se construyen a partir de un conjunto de simbolos y constantes numericas
data PExpr s = Number Number 
                | Mul [PExpr s] 
                | Add [PExpr s] 
                | Pow (PExpr s) (PExpr s) 
                | Fun s [PExpr s]

instance Eq s => Eq (PExpr s) where
    (Number a) == (Number b) = a == b
    (Mul a) == (Mul b) = a == b
    (Add a) == (Add b) = a == b
    (Pow a b) == (Pow c d) = a == c && b == d
    (Fun a b) == (Fun c d) = a == c && b == d
    _ == _ = False

instance Ord s => Ord (PExpr s) where

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

    u <= v = u<v || u==v

unquote :: String -> String
unquote ('"':s) | last s == '"' = init s
unquote s = s

paren :: String -> String
paren s = "(" ++ s ++ ")"


-- TODO: usar DOC
instance Show s => Show (PExpr s) where
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
    show (Add xs) = intercalate "+" $ map parenExpr xs
        where
            parenExpr (Number s)
                | s < 0 || not (isInteger s) = paren $ show s
                | otherwise = show s
            parenExpr s@(Add _) = paren $ show s
            parenExpr s@(Mul _) = paren $ show s
            parenExpr s = show s
    show (Pow x y) = parenExpr x ++ "^" ++ parenExpr y
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


instance Num (PExpr s) where
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
    negate (Mul ps) = Mul ((fromInteger (-1)):ps)
    negate e = Mul [fromInteger (-1), e]

    abs = undefined
    signum = undefined

instance Functor PExpr where
    fmap = liftM

instance Applicative PExpr where
    pure = return
    (<*>) = ap

instance Monad PExpr where
    return x = Fun x []
    Number x >>= _ = Number x
    Mul xs >>= f = Mul $ map (>>= f) xs
    Add xs >>= f = Add $ map (>>= f) xs
    Pow x y >>= f = Pow (x >>= f) (y >>= f)
    Fun s xs >>= f = f s >>= \s' -> Fun s' (map (>>= f) xs)


