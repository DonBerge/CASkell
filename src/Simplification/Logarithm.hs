{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Simplification.Logarithm
-- Description :  Simplificación de expresiones con funciones logaritmicas.
--
-- Sean \(u,v\) y \(w\) expresiones, con \(u\) y \(v\) positivos, la función logaritmo satisface las siguientes propiedades:
--
-- \[
--     \begin{align*}
--         \log(u\cdot v) &= \log(u) + \log(v) \\
--         \log(u^w) &= w\cdot \log(u) \\
--     \end{align*}
-- \]
--
-- La operacion que aplica las propiedades de izquierda a derecha se llama /expansión logaritmica/ y la operación que las aplica de derecha a izquierda
-- se llama /contracción logaritmica/. Este modulo contiene definiciones tanto para la expansión como para la contracción de logaritmos.

module Simplification.Logarithm where

import Expr
import Structure
import qualified Simplification.Algebraic as Algebraic
import PExpr (Assumptions(isPositive), true)
import TwoList (partition)

-- $setup
-- >>> let x = assume (symbol "x") ["positive"]
-- >>> let y = assume (symbol "y") ["positive"]
-- >>> let z = assume (symbol "z") ["positive"]
-- >>> let w = assume (symbol "w") ["positive"]
-- >>> let a = symbol "a"
-- >>> let b = symbol "b"

-- * Expansión de logaritmos
{-|
    Una expresión algebraica \(u\) esta en forma logaritmica expandida si el argumento de cada función logaritmica en \(u\):
    
        1. No es un producto.
        2. No es un exponente.

    >>> expand (log(a*b*x*y))
    Log(a*b)+Log(x)+Log(y)

    >>> expand (log((w*x)**a) + log(y**b * z))
    Log(z)+(Log(w)+Log(x))*a+Log(y)*b
-}
expand :: Expr -> Expr
expand (mapStructure expand -> v) = case structure v of
    Log w -> expandRules w
    _ -> v
    where
        expandRules (structure -> Mul xs) = let (a,b) = partition (true . isPositive) xs in sum (fmap expandRules a) + log (product b)
        expandRules (structure -> Pow x y)
            | true (isPositive x) = y * expandRules x
        expandRules v' = log v'

-- * Contración de logarithmos