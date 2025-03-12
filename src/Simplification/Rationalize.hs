{-# LANGUAGE ViewPatterns #-}

{-|
    Module      : Simplification.Rationalize
    Description : Módulo que racionaliza y simplifica expresiones racionales.
-}
module Simplification.Rationalize (
    rationalize,
    cancel
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
  'cancel' comvierte una expresión en su forma racional simplificada

  Una expresión \(u\) esta en forma racional simplificada si:

    1. \(u\) es un polinomio multivariable con coeficientes enteros
    2. Sea \(n\) y \(d\) el numerador y el denominador de \(u\) entonces:
      2.1 \(n\) y \(d\) son polinomios multivariables con coeficientes enteros.
      2.2 \(d \neq 0\) y \(d \neq 1\).
      2.3 \(n\) y \(d\) son coprimos.
      2.4 \(n\) y \(d\) estan normalizados.
    3. \(u=(-1)v\) donde \(v\) cumple la condición 2.

  === Ejemplos:

  >>> cancel $ (-4*a**2 + 4*b**2) / (8*a**2 - 16*a*b + 8*b**2)
  (-a-b)/(2*a-2*b)

  >>> cancel $ (1/(1/a + c/(a*b))) + ((a*b*c+a*c**2)/((b+c)**2))
  a

  >>> cancel $ (2*a**3 + 22*a*b + 6*a**2 + 7*a + 6*a**2*b + 12*b**2 + 21*b) / (7*a**2 - 2*a**2*b - 5*a - 5*a*b**2 + 21*a*b - 15*b + 3*b**3)
  (-2*a^2-6*a-4*b-7)/(2*a*b-7*a-b^2+5)
-}
cancel :: Expr -> Expr
cancel u = let
            u' = rationalize u
            n = Algebraic.expand $ numerator u'
            d = Algebraic.expand $ denominator u'
            -- Variables en la expresion expresion
            v = variables n `union` variables d
            ggcd = polyGCD n d v
            -- Dividir el numerador y el denominador por el gcd de ambos
            n' = recQuotient n ggcd v
            d' = recQuotient d ggcd v
           in
               cancelNumberAndSign n' d' v

numberCoefficient :: Expr -> Number.Number
numberCoefficient (Number p) = p
numberCoefficient (Mul ((Number p) :|| _)) = p
numberCoefficient _ = 1

-- Obtener los coeficientes que sean numeros
numberCoefficientList :: Expr -> [Number.Number]
numberCoefficientList (Number p) = [p]
numberCoefficientList (Mul ((Number p) :|| _)) = [p]
numberCoefficientList (Add us) = concatMap numberCoefficientList us
numberCoefficientList _ = [1]

-- | Elimina los coeficientes fraccionarios del numerador y del denominador
-- Esto se consigue dividiendo el numerador y el denominador por la constante c
-- c es el cociente entre el gcd de los numeradores de los coeficientes numericos y 
-- el lcm de los denominadores de los coeficientes numericos de la expresion
-- El resultado son un nuevo numerador y denominador, con coeficientes numericos enteros
-- y coprimos entre si

-- Este proceso de simplificacion obtiene el mismo resultado que obtendria 'rationalize'
-- sin embargo cuando se trata de coeficientes numericos, el resultado de 'rationalize'
-- es anulado por la autosimplificación.
cancelNumbers :: Expr -> Expr -> (Expr, Expr)
cancelNumbers _ 0 = (undefinedExpr "Division by zero", undefinedExpr "Division by zero")
cancelNumbers 0 _ = (0,1)
cancelNumbers n d = let
                        c = numberCoefficientList n ++ numberCoefficientList d
                        -- Obtener el el gcd de los numeradores y el lcm de los denominadores
                        (n',d') = foldr (\x -> bimap (gcd (Number.numerator x)) (lcm (Number.denominator x))) (0,1) c
                        c' = fromInteger n' / fromInteger d'
                     in 
                        (n / c', d / c')

-- | Cancela signos de manera que el numerador y el denominador tengan signo positivo
-- | o bien el numerador tiene signo negativo y el denominador positivo
cancelSign :: Expr -> Expr -> [Expr] -> Expr
cancelSign n d v = let
                        mlcm = numberCoefficient $ mostLeadingCoefficient d v  -- obtener el coeficiente numerico principal del denominador
                        negd = signum mlcm == -1
                     in if negd
                            then (-n) / (-d)
                            else n / d 
cancelNumberAndSign :: Expr -> Expr -> [Expr] -> Expr
cancelNumberAndSign n d v = let
                                (n',d') = cancelNumbers n d
                              in
                                cancelSign n' d' v