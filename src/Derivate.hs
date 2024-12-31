{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Derivate where 
import Expr

import PExpr

import Symplify

expr :: PExpr -> Expr
expr = return

derivate :: Expr -> String -> Expr
derivate f x = do
                f' <- f
                x' <- symbol x
                derivate' f' x'

mapIndex :: Int -> (a->a) -> [a] -> [a]
mapIndex _ _ [] = []
mapIndex 0 f (x:xs) = f x : xs
mapIndex n f (x:xs) = x : mapIndex (n-1) f xs


derivatives :: [(String, Expr -> Expr)]
derivatives = [
                ("Sin", \x -> cos x),
                ("Cos", \x -> -sin x),
                ("Tan", \x -> 1 / cos x ** 2),
                ("Exp", \x -> exp x),
                ("Log", \x -> 1 / x),
                ("Sqrt", \x -> 1 / (2 * sqrt x)),
                ("Asin", \x -> 1 / sqrt (1 - x ** 2)),
                ("Acos", \x -> -1 / sqrt (1 - x ** 2)),
                ("Atan", \x -> 1 / (1 + x ** 2)),
                ("Sinh", \x -> cosh x),
                ("Cosh", \x -> sinh x),
                ("Tanh", \x -> 1 / cosh x ** 2),
                ("Asinh", \x -> 1 / sqrt (x ** 2 + 1)),
                ("Acosh", \x -> 1 / sqrt (x ** 2 - 1)),
                ("Atanh", \x -> 1 / (1 - x ** 2))
              ]

derivate' :: PExpr -> PExpr -> Expr
derivate' (Add ps) x = mapM (`derivate'` x) ps >>= simplifySum
--derivate' (Mul ps) x
derivate' (Pow pa pb) x = let
                            a = expr pa
                            b = expr pb 
                            a' = derivate' pa x
                            b' = derivate' pb x
                          in
                            (a ** b) * (b' * log a + b * (a'/a))
derivate' (Fun f [p]) x = let
                            p' = derivate' p x
                          in
                            case lookup f derivatives of
                              Just f' -> f' (expr p) * p'
                              -- TODO: debe devolverse junto a un operador de derivada
                              Nothing -> fail "Derivative of unknown function"
derivate' s x = if s == x then return 1 else return 0