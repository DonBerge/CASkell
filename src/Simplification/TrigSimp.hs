{-# LANGUAGE PatternSynonyms #-}
module Simplification.TrigSimp where

import PExpr

import Symplify (simplifyProduct, simplifySum)
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

trigSubstitute :: PExpr -> Expr
trigSubstitute = bottomUp sub'
    where
        sub' (Sec x) = recip $ cos $ expr x
        sub' (Csc x) = recip $ sin $ expr x
        sub' (Tan x) = sin (expr x) / cos (expr x)
        sub' (Cot x) = cos (expr x) / sin (expr x)
        sub' x = expr x