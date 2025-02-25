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
import PExpr (Assumptions(isPositive), true)
import Data.TwoList (partition)

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

    Ejemplos:

    >>> expand (log(x/y))
    Log(x)+(-1)*Log(y)

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

-- * Contración de logaritmos

{-|
    Una expresión algebraica \(u\) esta en forma logaritmica contraida si satisface estas 2 propiedades:
    
        1. Una suma en \(u\) contiene como mucho un logaritmo.
        2. Un producto en \(u\) contiene como mucho un logaritmo.

    Ejemplos:

    >>> contract (log(x)+log(y)+log(z))
    Log(x*y*z)

    >>> contract (2*log(a)*log(x))
    Log(x^Log(a^2))
-}
contract :: Expr -> Expr
contract (mapStructure contract -> v)
    | sumOrMul v = contractRules v
    | otherwise = v
    where
        sumOrMul (structure -> Add _) = True
        sumOrMul (structure -> Mul _) = True
        sumOrMul _ = False

        isLog (structure -> Log _) = True
        isLog _ = False

        contractRules (structure -> Add xs) = let (a,b) = foldl combineSum (1,0) xs in log a + b
        contractRules (structure -> Mul xs) = 
            case partition isLog xs of
                ([],factors) -> product factors
                ([structure -> Log x], factors) -> log (x**(product factors))
                (((structure -> Log y):ys), factors) -> foldl1 combinelogs ((log (y**(product factors))) : ys)
                _ -> error "Este caso nunca se va a dar"
        contractRules v' = log v'

        combinelogs l (structure -> Log x) = log(x**l)
        combinelogs _ _ = error "Este caso nunca se va a dar"

        -- Obtiene el producto de los argumentos de los logaritmos y la suma de los terminos que no tienen logaritmos
        combineSum (a,b) (structure -> Log x) = (a*x,b)
        combineSum (a,b) x = (a,b+x)