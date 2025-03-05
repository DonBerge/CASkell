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
-- La operacion que aplica las propiedades de izquierda a derecha se llama /expansión exponencial/ y la operación que las aplica de derecha a izquierda
-- se llama /contracción exponencial/. Este modulo contiene definiciones tanto para la expansión como para la contracción de exponenciales.
module Simplification.Exponential where

import Assumptions
import Expr
import qualified Simplification.Algebraic as Algebraic
import Simplification.Rationalize (rationalize)

-- $setup
-- >>> let x = symbol "x"
-- >>> let y = symbol "y"
-- >>> let z = symbol "z"
-- >>> let w = symbol "w"

-- bottomUp :: (Expr -> Expr) -> Expr -> Expr
-- bottomUp f = f . mapStructure (bottomUp f)

--  Sustitución de exponenciales

--
--    Reemplaza las ocurrencias de 'sinh', 'cosh', 'tanh', 'cot', 'sec' y 'csc' por sus equivalentes en seno y coseno.
--
--    === Ejemplos
--
--    >>> expSubstitute (sinh x)
--    (-1/2)*Exp((-1)*x)+1/2*Exp(x)
--    >>> expSubstitute (cosh x)
--    1/2*Exp((-1)*x)+1/2*Exp(x)
--    >>> expSubstitute (csch x + sech y)
--    2*((-1)*Exp((-1)*x)+Exp(x))^(-1)+2*(Exp((-1)*y)+Exp(y))^(-1)
-- expSubstitute :: Expr -> Expr
-- expSubstitute = bottomUp expSubstitute'
--   where
--     expSubstitute' (Sinh x) = (exp x - exp (-x)) / 2
--     expSubstitute' (Cosh x) = (exp x + exp (-x)) / 2
--     expSubstitute' (Tanh x) = (exp x - exp (-x)) / (exp x + exp (-x))
--     expSubstitute' (Coth x) = (exp x + exp (-x)) / (exp x - exp (-x))
--     expSubstitute' (Sech x) = 2 / (exp x + exp (-x))
--     expSubstitute' (Csch x) = 2 / (exp x - exp (-x))
--     expSubstitute' x = x

-- * Expansion de exponenciales

-- |
--    Sea \(u=\prod u_i\), 'separateIntegerTerms' separa aquellos terminos \(u_i\) que son enteros de aquellos que no lo son.
--
--    Si \(u\) no es un producto, se aplica la operación viendo a \(u\) como un producto entre 1 y si mismo.
--
--    >>> separateIntegerTerms (2*x*(y+z))
--    (2,x*(y+z))
separateIntegerTerms :: Expr -> (Expr, Expr)
separateIntegerTerms (Mul xs) = foldl combine (1, 1) xs
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
--        >>> expand (exp (2*w*x + 3*y*z))
--        (e^(w*x))^2*(e^(y*z))^3
--
--        >>> expand (exp(x+y) ** 2)
--        (e^x)^2*(e^y)^2
--
--        >>> expand (1 / (exp(2*x) - exp(x)**2))
--        Undefined: Division por cero
-- 
--        >>> expand (exp((x+y)*(x-y)))
--        e^(x^2)/e^(y^2)
--
--        >>> expand (exp((x+y)**2))
--        e^(x^2)*(e^(x*y))^2*e^(y^2)

-- Primero se expanden las subexpresiones algebraicas
expand :: Expr -> Expr
expand (Algebraic.expandMainOp . mapStructure expand -> v) = case v of
  Exp w -> expandRules w -- Si la expresión raiz es una exponencial, aplicar las propiedades de expansión
  _ -> v
  where
    expandRules (Algebraic.expandMainOp -> v') = case v' of
      Add us -> product $ fmap expandRules us -- Propiedad de expansión para sumas
      Mul _ -> let (a, b) = separateIntegerTerms v' in (exp b) ** a -- Propiedad de expansión para productos
      _ -> exp v' -- Si no es una suma o producto, no se puede expandir

-- * Contracción de exponenciales

{-|
    Una expresión algebraica \(u\) esta en forma exponencial contraida si satisface las siguientes propiedades:

        1. Todo producto en \(u\) contiene como mucho un operando que es una función exponencial.
        2. Toda potencia en \(u\) no tiene una función exponencial como base.
        3. Toda subexpresión de \(u\) esta en forma algebraica expandida.
    
    Ejemplos:

      >>> contract (exp(x) * (exp(x)+exp(y)))
      e^(2*x)+e^(x+y)

      >>> contract (exp(exp(x)) ** exp(y))
      e^(e^(x+y))

-}
contract :: Expr -> Expr
contract (Algebraic.expandMainOp . mapStructure contract -> v)
  | mulOrPow v = contractRules v -- contraer exponenciales en productos o potencias
  | otherwise = v
    where
      mulOrPow (Mul _) = True
      mulOrPow (Pow _ _) = True
      mulOrPow _ = False

      contractRules (Algebraic.expandMainOp -> v') = case v' of
        Pow b s -> case b of
                    Exp b' -> let p = b'*s in if mulOrPow p then exp(contractRules p) else exp p
                    _ -> v'
        Mul vs -> let (p,s) = foldl combineMul (1,0) vs in (exp s) * p
        Add vs -> foldl combineSum 0 vs
        _ -> v'

      -- La expansión de expandMainOp puede introducir sumas, por lo que se deben contraer los sumandos
      combineSum s y
        | mulOrPow y = s + contractRules y
        | otherwise = s + y

      -- combinar los argumentos de las exponenciales en s, usando la propiedad exp(s)*exp(y) = exp(s+y)
      -- los argumentos que no son exponenciales se multiplican en p
      combineMul (p,s) (Exp y) = (p, s+y)
      combineMul (p,s) y = (p*y, s)

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
