{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-|
    Module      : Simplification.Algebraic
    Description : Módulo que implementa la expansión de expresiones algebraicas.
-}
module Simplification.Algebraic (
    expand,
    expandMainOp
) where


import Expr.ExprType
import Expr.Structure

import Math.Combinatorics.Exact.Binomial (choose)
-- $setup
-- >>> import Expr.PrettyPrint
-- >>> x = symbol "x"
-- >>> y = symbol "y"
-- >>> z = symbol "z"
-- >>> a = symbol "a"
-- >>> b = symbol "b"
-- >>> c = symbol "c"
-- >>> d = symbol "d"

{-|
    Expansión de expresiones, una expresion esta expandida si ninguna suma es el operando de un producto o de una potencia
    a exponente entero.

    == Ejemplos
    
    >>> expand ((x+2)*(x+3)*(x+4))
    x^3+9*x^2+26*x+24
    >>> expand ((x+y+z)**3)
    x^3+3*x^2*y+3*x^2*z+3*x*y^2+6*x*y*z+3*x*z^2+y^3+3*y^2*z+3*y*z^2+z^3
    >>> expand ((x+1)**2+(y+1)**2)
    x^2+2*x+y^2+2*y+2
    >>> expand (((x+2)**2 + 3)**2)
    x^4+8*x^3+30*x^2+56*x+49
    >>> expand (sin(a*(b+c)))
    sin(a*b+a*c)
    >>> expand (a/(b*(c+d)))
    a/(b*c+b*d)

    La expansion algebraica tambien expande expresiones con exponentes fraccionarios:
    
    >>> expand ((x+1)**(5/2))
    √(x+1)*x^2+2*√(x+1)*x+√(x+1)

    La expansion anterior se realiza haciendo la transformación \((1+x)^{5/2} = (1+x)^{1/2}(x+1)^2\) y luego expandiendo \((x+1)^2\)

-}
expand :: Expr -> Expr
expand (Add xs) = expandFraction $ sum $ fmap expand xs
expand (Mul xs) = foldr1 expandProduct $ fmap expand xs
expand (Pow b (Number f)) = let
                              b' = expand b
                              (fl, m) = properFraction f
                            in 
                               expandProduct (expandPower b' fl) (b' ** (fromNumber m))
expand u@(Fun _ _) = mapStructure expand u
expand u = expandFraction u

-- | Expansion utilizando la propiedad distributiva
expandProduct :: Expr -> Expr -> Expr
expandProduct (Add rs) s = expand (sum $ fmap (`expandProduct` s) rs) -- auto simplificacion puede generar terminos no expandidos, hay que expandir de nuevo
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = expandFraction $ r * s 

-- | Expansion utilizando el binomio de newton
expandPower :: Expr -> Integer -> Expr
expandPower u n
    | n < 0 = 1 / expandPower u (-n)
expandPower _ 0 = 1
expandPower u 1 = u
expandPower (Add (f :|| rs)) n = let
                                    rs' = sum rs
                                 in
                                    sum [expandProduct (cf k) (expandPower rs' (n-k)) | k <- [0..n]]
    where
        cf k = (fromInteger $ choose n k) * f ** (fromInteger k)
expandPower u n = u ** (fromInteger n)

-- | Expande el denominador de una expresion
expandFraction :: Expr -> Expr
expandFraction x = let
                    n = numerator x
                    d = denominator x
                   in if d == 1
                    then x
                    else n / (expand d)

{-|
    Expande la expresion solo una vez, las subexpresiones no se expanden

    === Ejemplos:
    
    >>> expandMainOp (x*(2+(1+x)**2))
    (x+1)^2*x+2*x
    >>> expandMainOp ((x+(1+x)**2)**2)
    x^2+2*(x+1)^2*x+(x+1)^4
-}
expandMainOp :: Expr -> Expr
expandMainOp (Mul xs) = foldr1 expandProduct' xs
    where
        expandProduct' u (Add us) = sum $ fmap (u*) us
        expandProduct' u@(Add _) v = expandProduct' v u
        expandProduct' u v = u * v
expandMainOp (Pow b (Number f)) = let
                                    (fl, m) = properFraction f
                                  in 
                                    (expandPower b fl) * (b ** (fromNumber m))
expandMainOp u = expandFraction u