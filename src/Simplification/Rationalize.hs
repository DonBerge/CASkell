{-# LANGUAGE ViewPatterns #-}

module Simplification.Rationalize (
    rationalize,
    rationalSimplify,
) where

import Expr
import Data.Bifunctor
import Data.List

import qualified Data.Number as Number
import qualified Simplification.Algebraic as Algebraic

-- $setup
-- >>> x = symbol "x"
-- >>> a = symbol "a"
-- >>> b = symbol "b"
-- >>> c = symbol "c"

{-|
  'rationalize' combina los numeradores y denominadores de los operadores en una expresión

  >>> u = 1 + 1/(1+1/x)
  >>> rationalize u
  (2*x+1)/(x+1)

  >>> (numerator u, denominator u)
  (1/(1/x+1)+1,1)

  >>> (numerator (rationalize u), denominator (rationalize u))
  (2*x+1,x+1)

-}
rationalize :: Expr -> Expr
rationalize (Pow x y) = (rationalize x) ** y
rationalize (Mul xs) = product $ fmap rationalize xs
rationalize u@(Add (f :|| _)) = let
                                                g = rationalize f
                                                h = rationalize (u - f)
                                             in
                                                rationalizeSum g h
rationalize u = u

-- Combina los denominadores de dos expresiones sumadas
rationalizeSum :: Expr -> Expr -> Expr
rationalizeSum u v = let
                        m = numerator u
                        r = denominator u
                        n = numerator v
                        s = denominator v
                      in
                        if r == 1 && s == 1
                            then u + v
                            else (rationalizeSum (m*s) (n*r)) / (r*s)

{-|
  'rationalSimplify' comvierte una expresión en su forma racional simplificada

  Una expresión \(u\) esta en forma racional simplificada si:

    1. \(u\) es un polinomio multivariable con coeficientes enteros
    2. Sea \(n\) y \(d\) el numerador y el denominador de \(u\) entonces:

      2.1 \(n\) y \(d\) son polinomios multivariables con coeficientes enteros.
      2.2 \(d \neq 0\) y \(d \neq 1\).
      2.3 \(n\) y \(d\) no tienen factores comunes.
      2.4 \(n\) y \(d\) estan normalizados.
    3. \(u=(-1)v\) donde \(v\) cumple la condición 2.

  === Ejemplos:

  >>> rationalSimplify $ (-4*a**2 + 4*b**2) / (8*a**2 - 16*a*b + 8*b**2)
  (a+b)/(-2*a+2*b)

  >>> rationalSimplify $ (1/(1/a + c/(a*b))) + ((a*b*c+a*c**2)/((b+c)**2))
  a

  >>> rationalSimplify $ (2*a**3 + 22*a*b + 6*a**2 + 7*a + 6*a**2*b + 12*b**2 + 21*b) / (7*a**2 - 2*a**2*b - 5*a - 5*a*b**2 + 21*a*b - 15*b + 3*b**3)
  (-2*a^2-6*a-4*b-7)/(2*a*b-7*a-b^2+5)
-}
rationalSimplify :: Expr -> Expr
rationalSimplify u = let
                        u' = rationalize u
                        n = Algebraic.expand $ numerator u'
                        d = Algebraic.expand $ denominator u'
                        v = variables n `union` variables d
                        ggcd = polyGCD n d v
                        n' = recQuotient n ggcd v
                        d' = recQuotient d ggcd v
                    in
                        simplifyNumberAndSign n' d' v
    where
        -- Obtener los coeficientes que sean numeros
        numberCoefficientList (Number p) = [p]
        numberCoefficientList (Mul ((Number p) :|| _)) = [p]
        numberCoefficientList (Add us) = concatMap numberCoefficientList us
        numberCoefficientList _ = [1]

        -- signNormalized = normalized

        simplifyNumbers _ 0 = (undefinedExpr "Division by zero", undefinedExpr "Division by zero")
        simplifyNumbers 0 _ = (0,1)
        simplifyNumbers n d = let
                                c = numberCoefficientList n ++ numberCoefficientList d
                                -- Obtener el lcm de los denominadores y el gcd de los numeradores
                                (n',d') = foldr (\x -> bimap (gcd (Number.numerator x)) (lcm (Number.denominator x))) (0,1) c  -- n' is lcm of the denominators and d' is the gcd of the numerators
                                c' = fromInteger n' / fromInteger d'
                             in 
                                (n / c', d / c')
        
        simplifySign n d v = let
                                normn = normalized n v
                                normd = normalized d v
                                n' = if normn then n else negate n
                                d' = if normd then d else negate d
                             in
                                if normn == normd -- if both n and d had the same sign, do nothing otherwise negate the quotient
                                    then n' / d'
                                    else negate (n' / d')
        simplifyNumberAndSign n d v = let
                                        (n',d') = simplifyNumbers n d
                                      in
                                        simplifySign n' d' v