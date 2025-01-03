{-# LANGUAGE PatternSynonyms #-}
module Simplification.TrigSimp where

import PExpr

import Symplify (simplifyProduct, simplifySum, isInteger)
import Expr

pattern Recip :: PExpr -> PExpr
pattern Recip x = Pow x (-1)

pattern Div :: PExpr -> PExpr -> PExpr
pattern Div x y = Mul [x, Recip y]

pattern RDiv :: PExpr -> PExpr -> PExpr
pattern RDiv x y = Mul [Recip x, y]

pattern DivPow :: PExpr -> PExpr -> PExpr -> PExpr -> PExpr
pattern DivPow x y a b = Mul [Pow x a, Pow y b]

-- Funciones basadas em "AUTOMATED AND READABLE SIMPLIFICATION OF TRIGONOMETRIC EXPRESSIONS" de Hongguang Fu
expr :: PExpr -> Expr
expr = return

bottomUp :: (PExpr -> Expr) -> PExpr -> Expr
bottomUp f (Add xs) = mapM (bottomUp f) xs >>= simplifySum >>= f
bottomUp f (Mul xs) = mapM (bottomUp f) xs >>= simplifyProduct >>= f
bottomUp f (Pow x y) = ((bottomUp f x) ** (bottomUp f y)) >>= f
bottomUp _ x@(Symbol _) = expr x
bottomUp f (Fun x xs) = (Fun x <$> mapM (bottomUp f) xs) >>= f
bottomUp _ x = expr x

tr1 :: PExpr -> Expr
tr1 = bottomUp tr1'
    where
        tr1' (Sec x) = recip $ cos $ expr x
        tr1' (Csc x) = recip $ sin $ expr x
        tr1' x = expr x

tr2 :: PExpr -> Expr
tr2 = bottomUp tr2'
    where
        tr2' (Tan x) = sin (expr x) / cos (expr x)
        tr2' (Cot x) = cos (expr x) / sin (expr x)
        tr2' x = expr x

tr2i :: PExpr -> Expr
tr2i = bottomUp tr2i'
    where
        tr2i' (RDiv x y) = tr2i' (Div y x) 
        tr2i' (Div (Sin x) (Cos y))
            | x == y = tan $ expr x
        tr2i' (Div (Cos x) (Sin y))
            | x == y = cot $ expr x
        tr2i' (DivPow x y a b)
            | isInteger a && a == -b = tr2i (Div x y) ** (expr a)
        tr2i' x = expr x