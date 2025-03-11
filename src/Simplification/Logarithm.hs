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
import Assumptions
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
    
        1. No es un producto de expresiones positivas.
        2. No es un exponente con base positiva.

    Ejemplos:

    >>> logExpand (log(x/y))
    log(x)-log(y)

    >>> logExpand (log(a*b*x*y))
    log(a*b)+log(x)+log(y)

    >>> logExpand (log((w*x)**a) + log(y**b * z))
    a*(log(w)+log(x))+b*log(y)+log(z)
-}
logExpand :: Expr -> Expr
logExpand (mapStructure logExpand -> v) = case v of
    Log w -> expandRules w
    _ -> v
    where
        expandRules (Mul xs) = let (a,b) = partition (true . isPositive) xs in sum (fmap expandRules a) + log (product b)
        expandRules (Pow x y)
            | true (isPositive x) = y * expandRules x
        expandRules v' = log v'

-- * Contración de logaritmos

{-|
    Una expresión algebraica \(u\) esta en forma logaritmica contraida si satisface estas 2 propiedades:
    
        1. Una suma en \(u\) contiene como mucho un logaritmo.
        2. Un producto en \(u\) contiene como mucho un logaritmo.

    Ejemplos:

    >>> logContract (log(x)+log(y)+log(z))
    log(x*y*z)

    >>> logContract (2*log(a)*log(x))
    log(x^log(a^2))
-}
logContract :: Expr -> Expr
logContract (mapStructure logContract -> v)
    | sumOrMul v = contractRules v
    | otherwise = v
    where
        sumOrMul (Add _) = True
        sumOrMul (Mul _) = True
        sumOrMul _ = False

        isLog (Log _) = True
        isLog _ = False

        contractRules (Add xs) = let (a,b) = foldl combineSum (1,0) xs in log a + b
        contractRules (Mul xs) = 
            case partition isLog xs of
                ([],factors) -> product factors
                ([Log x], factors) -> log (x**(product factors))
                (((Log y):ys), factors) -> foldl1 combinelogs ((log (y**(product factors))) : ys)
                _ -> error "Este caso nunca se va a dar"
        contractRules v' = log v'

        combinelogs l (Log x) = log(x**l)
        combinelogs _ _ = error "Este caso nunca se va a dar"

        -- Obtiene el producto de los argumentos de los logaritmos y la suma de los terminos que no tienen logaritmos
        combineSum (a,b) (Log x) = (a*x,b)
        combineSum (a,b) x = (a,b+x)

logSimplify :: Expr -> Expr
logSimplify = undefined