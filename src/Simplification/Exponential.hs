{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Simplification.Exponential
-- Description :  Simplificación de expresiones con funciones exponentiales.
--
-- Sean \(u,v\) y \(w\) expresiones, la función exponencial satisface las siguientes propiedades:
--
-- \[
--     \begin{align*}
--         \exp(u+v) &= \exp(u) \cdot \exp(v) \\
--         \exp(w \cdot u) &= \exp(u)^w \\
--     \end{align*}
-- \]
--
-- La operacion que aplica las propiedades de izquierda a derecha se llama *expansión exponencial* y la operación que las aplica de derecha a izquierda
-- se llama *contracción exponencial*. Este modulo contiene definiciones tanto para la expansión como para la contracción de exponenciales.
module Simplification.Exponential where

import Classes.Assumptions
import Expr
import qualified Simplification.Algebraic as Algebraic
import Structure
import Simplification.Rationalize (rationalize)

-- * Expansion de exponenciales

-- |
--    Sea \(u=\prod u_i\), 'separateIntegerTerms' separa aquellos terminos \(u_i\) que son enteros de aquellos que no lo son.
--
--    Si \(u\) no es un producto, se aplica la operación viendo a \(u\) como un producto entre 1 y si mismo.
separateIntegerTerms :: Expr -> (Expr, Expr)
separateIntegerTerms (structure -> Mul xs) = foldl combine (1, 1) xs
  where
    combine (a, b) (separateIntegerTerms -> (c, d)) = (a * c, b * d)
separateIntegerTerms u
  | true (isInteger u) = (u, 1)
  | otherwise = (1, u)

-- |
--    Una expresión \(u\) esta en forma exponencial expandida si el argumento de cada función exponencial en \(u\):
--
--        1. No es una suma.
--        2. No es un producto con un operando entero.
--        3. Cualquier subexpresión de \(u\) esta en forma algebraica expandida.
--
--    La propiedad \(exp(w \cdot u) = exp(u)^w\) provee una forma de remover cualquier opeando de un producto que es argumento de una función
--    exponencial, pero no especifica cual remover. Para eliminar esta ambiguedad solo se remueven los operandos enteros de un producto.
--
--    Ejemplos:
--
--        > expand (exp (2*w*x + 3*y*z)) = exp(w*x)^2 * exp(y*z)^3
--        > expand (exp (x+y)^2) =  exp(x)^2 * exp(y)^2
--        > expand (1 / (exp (2*x) - exp(x)^2)) = Undefined: Division por cero
--        > expand (exp((x+y)(x-y))) = exp(x^2) / exp(y^2)
--        > expand (exp((x+y)^2)) = exp(x^2) * exp(x*y)^2 * exp(y^2)

-- Primero se expanden las subexpresiones algebraicas
expand :: Expr -> Expr
expand (mapStructure expand . Algebraic.expand -> v) = case structure v of
  Exp w -> expandRules w -- Si la expresión raiz es una exponencial, aplicar las propiedades de expansión
  _ -> v
  where
    expandRules (structure -> Add us) = product $ fmap expandRules us -- Propiedad de expansión para sumas
    expandRules u@(structure -> Mul _) =
      -- Propiedad de expansión para productos
      let (a, b) = separateIntegerTerms u
       in (exp b) ** a
    expandRules u = exp u -- Si no es una suma o producto, no se puede expandir

-- * Contracción de exponenciales

contract :: Expr -> Expr
contract = undefined

-- * Simplificación de exponenciales

{-|
    Simplifica expresiones con exponenciales primero racionalizando la expresión y luego contrayendo el numerador y el denominador.
-}
simplify :: Expr -> Expr
simplify u = let
                u' = rationalize u
                n = contract $ numerator u'
                d = contract $ denominator u'
             in
                n / d
