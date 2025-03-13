{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-|
    Module      : Simplification.Trigonometric
    Description : Funciones para simplificar expresiones trigonométricas.
-}
module Simplification.Trigonometric (
  trigExpand,
  trigContract,
  trigSimplify
) where

import Calculus.Utils
import Assumptions
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.Matrix as Matrix
import Expr
import Math.Combinatorics.Exact.Binomial (choose)
import qualified Simplification.Algebraic as Algebraic
import Simplification.Rationalize (rationalize)
import Data.Foldable (minimumBy)
import Data.Function (on)

-- $setup
-- >>> let x = symbol "x"
-- >>> let y = symbol "y"
-- >>> let z = symbol "z"

-- * Expansion de expresiones trigonometricas

-- |
--    Convierte expresiones en su forma trigonometrica expandida.
--
--    Una expresion esta en forma trigonometrica expandida si cada argumento de un seno o coseno cumple que:
--
--        1. No es una suma;
--
--        2. No es un producto con un operando que es un entero.
--
--    === Propiedades utilizadas:
--    \(\sin(v+w) = \sin(v)\cos(w)+\cos(v)\sin(w)\)
--
--    \(\cos(v+w) = \cos(v)\cos(w)-\sin(v)\sin(w)\)
--
--    \(\cos(n \cdot x) = T_n(\cos x)\), donde \(T_n\) es el [polinomio de Chebyshev de primera clase de grado \(n\)](https://en.wikipedia.org/wiki/Chebyshev_polynomials#).
--
--    \(U_n(\cos x)\sin x = {\sin((n+1)x)}\), donde \(U_n\) es el [polinomio de Chebyshev de segunda clase de grado \(n\)](https://en.wikipedia.org/wiki/Chebyshev_polynomials#).
--
--    === Ejemplos :
--    
--    >>> trigExpand (cos (2*x))
--    2*cos(x)^2-1
--
--    >>> trigExpand (sin (3*y))
--    -4*sin(y)^3+3*sin(y)
--
--    >>> trigExpand (sin (x+y))
--    cos(x)*sin(y)+cos(y)*sin(x)
--
--    >>> trigExpand (3/4*sin(x)+3/4*cos(x+pi/6)-3/4*sin(x+pi/3))
--    0
trigExpand :: Expr -> Expr
trigExpand = Algebraic.expand . trigExpand' . mapStructure trigExpand
  where
    trigExpand' (Sin x) = fst $ expandTrigRules x
    trigExpand' (Cos x) = snd $ expandTrigRules x
    trigExpand' x = x

-- |
--  'expandTrigRules u' calcula la forma expandida de sin(u) y cos(u).
--  Devuelve el resultado en una tupla 
expandTrigRules :: Expr -> (Expr, Expr)
-- u = v + w
expandTrigRules u@(Add (x :|| _)) =
  let f = expandTrigRules x
      r = expandTrigRules (u - x)
      s = fst f * snd r + snd f * fst r -- sin(v)cos(w) + cos(v)sin(w)
      c = snd f * snd r - fst f * fst r -- cos(v)cos(w) - sin(v)sin(w)
   in (s, c)
expandTrigRules u@(Mul (f :|| _))
  | Number f' <- f,
    true (isInteger f') -- u = n * v
    =
      let f'' = toInteger f'
          u' = u / f
       in (multipleAngleSin f'' u', multipleAngleCos f'' u')
expandTrigRules u = (sin u, cos u)

-- |
--  Expansión de cos(n*x), utilizando los polinomios de Chebyshev de primera clase.
multipleAngleCos :: Integer -> Expr -> Expr
multipleAngleCos n u@(Add _) = trigExpand $ cos $ Algebraic.expandMainOp $ fromInteger n * u
multipleAngleCos 0 _ = 1
multipleAngleCos 1 x = cos x
multipleAngleCos n x
  | n < 0 = multipleAngleCos (-n) x
  | otherwise =
      let x' = symbol "_"
       in substitute (cheby1 x' n) x' (cos x)

-- |
--  Expansión de sin(n*x), utilizando los polinomios de Chebyshev de segunda clase.
multipleAngleSin :: Integer -> Expr -> Expr
multipleAngleSin n u@(Add _) = trigExpand $ sin $ Algebraic.expandMainOp $ fromInteger n * u
multipleAngleSin 0 _ = 0
multipleAngleSin 1 x = sin x
multipleAngleSin n x
  | n < 0 = -(multipleAngleSin (-n) x)
  | otherwise =
      let x' = symbol "_"
          f = substitute (cheby2 x' (n - 1)) x' (cos x)
          poly = mbPolyExpand f (1-cos x **2{-cos x ** 2-}) [cos x] (sin x ** 2) -- Sustituye (cos^2 x) por (1 - sin^2 x)

       in Algebraic.expandMainOp $ poly * sin x

-- |
--    Generación de polinomios de Chebyshev de primera clase en la variable x
cheby1 :: (Integral b) => Expr -> b -> Expr
cheby1 _ 0 = 1
cheby1 x 1 = x
cheby1 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2 * x, -1], [1, 0]] ^ (n - 1)) * Matrix.fromLists [[x], [1]]

-- |
--    Generación de polinomios de Chebyshev de segunda clase en la variable x
cheby2 :: (Integral b) => Expr -> b -> Expr
cheby2 _ 0 = 1
cheby2 x 1 = 2 * x
cheby2 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2 * x, -1], [1, 0]] ^ n) * Matrix.fromLists [[1], [0]]

-- * Contracion de expresiones trigonometricas

{- |
  Conversión de expresiones trigonometricas a su forma trigonometrica contraida.

  Una expresion esta en forma trigonometrica contraida si satisface que:

      1. Cualquier producto en la expresión tiene como mucho un operando que es un seno o coseno;
      2. Una potencia con exponente entero positivo no tiene como base a un seno o coseno;
      3. Cualquier subexpresión esta en forma expandida.

  === Propiedades utilizadas:
    \(\sin(v)\sin(w) = \frac{1}{2}(\cos(v-w)-\cos(v+w))\)

    \(\sin(v)\cos(w) = \frac{1}{2}(\sin(v+w)+\sin(v-w))\)

    \(\cos(v)\cos(w) = \frac{1}{2}(\cos(v-w)+\cos(v+w))\)

    \[
      \cos(v)^n = 
    \begin{cases} 
      \dfrac{\binom{n}{n/2}}{2^n} + \dfrac{1}{2^{n-1}}\sum_{j=0}^{n/2-1} \binom{n}{j} \cos((n-2j)v) & \text{si } n \text{ es par} \\
      \dfrac{1}{2^{n-1}}\sum_{j=0}^{\lfloor n/2 \rfloor} \binom{n}{j} \cos((n-2j)v) & \text{si } n \text{ es impar}
    \end{cases}\]

    \[
      \sin(v)^n =
    \begin{cases}
      \dfrac{(-1)^{n}}{2^n}\binom{n}{n/2} + \dfrac{(-1)^{n/2}}{2^{n-1}}\sum_{j=0}^{n/2-1} (-1)^j \binom{n}{j} \cos((n-2j)v) & \text{si } n \text{ es par} \\
      \dfrac{(-1)^{(n-1)/2}}{2^{n-1}}\sum_{j=0}^{\lfloor n/2 \rfloor} (-1)^j \binom{n}{j} \sin((n-2j)v) & \text{si } n \text{ es impar}
    \end{cases}
    \]

  === Ejemplos:
  >>> trigContract (sin(x)**2 * cos(x)**2)
  -cos(4*x)/8+1/8

  >>> trigContract (cos(x)**4)
  cos(2*x)/2+cos(4*x)/8+3/8
  
  >>> trigContract (sin(x)**2+cos(x)**2)
  1
  
  >>> trigContract ((cos(x) + sin(x))**4 + (cos(x) - sin(x))**4 + cos(4*x) - 3)
  0
  
  >>> trigContract (sin(x) + sin(y) - 2*sin(x/2 + y/2)*cos(x/2 - y/2))
  0
-}
trigContract :: Expr -> Expr
trigContract = trigContract' . mapStructure trigContract . trigContract'
  where
    trigContract' v@(Pow _ _) = contractTrigRules $ Algebraic.expandMainOp v
    trigContract' v@(Mul _) = contractTrigRules $ Algebraic.expandMainOp v
    trigContract' v = v

contractTrigRules :: Expr -> Expr
contractTrigRules v@(Pow _ _) = contractTrigPower v
contractTrigRules v@(Mul _) =
  let (c, d) = separateSinCos v
   in if d == 1
        then v
        else case d of
          Sin _ -> v
          Cos _ -> v
          Pow _ _ -> Algebraic.expandMainOp (c * contractTrigPower d)
          Mul ds -> Algebraic.expandMainOp (c * contractTrigProduct ds)
          _ -> undefinedExpr "Contract trig rules: Este caso no deberia ocurrir"
contractTrigRules (Add us) = sum $ fmap trigRules us
  where
    trigRules v@(Pow _ _) = contractTrigRules v
    trigRules v@(Mul _) = contractTrigRules v
    trigRules v = v
contractTrigRules v = v

-- |
--    Verifica que la expresión dada es un seno, coseno o una potencia entera de un seno o coseno
isSinOrCos :: Expr -> Bool
isSinOrCos (Sin _) = True
isSinOrCos (Cos _) = True
isSinOrCos (MonomialTerm v _)
  | isSinOrCos v = True
isSinOrCos _ = False

-- |
--    Dada una expresión @u@, 'separateSinCos' separa los senos y cosenos de los demás términos.
separateSinCos :: Expr -> (Expr, Expr)
separateSinCos (Mul (u :|| v :| us)) = bimap product product $ separateSinCos' (u : v : us)
  where
    separateSinCos' [] = ([], [])
    separateSinCos' (u : us)
      | isSinOrCos u = second (u :) $ separateSinCos' us
      | otherwise = first (u :) $ separateSinCos' us
separateSinCos u
  | isSinOrCos u = (1, u)
  | otherwise = (u, 1)

-- |
--   Contraccion de productos de funciones trigonometricas.
contractTrigProduct :: TwoList Expr -> Expr
contractTrigProduct (a :|| b :| [])
  | Pow _ _ <- a = contractTrigRules $ (contractTrigPower a) * b
  | Pow _ _ <- b = contractTrigRules $ a * contractTrigPower b
  | otherwise = case (a, b) of -- a y b son funciones trigonometricas
      (Sin p, Sin q) -> cos (p - q) / 2 - cos (p + q) / 2
      (Cos p, Cos q) -> cos (p - q) / 2 + cos (p + q) / 2
      (Sin p, Cos q) -> sin (p + q) / 2 + sin (p - q) / 2
      (Cos p, Sin q) -> sin (p + q) / 2 - sin (p - q) / 2
      _ -> a * b -- este caso no deberia darse
contractTrigProduct (a :|| b :| c : cs) = contractTrigRules $ a * contractTrigProduct (b :|| c :| cs)

-- |
--    Contraccion de potencias de funciones trigonometricas.
contractTrigPower :: Expr -> Expr
contractTrigPower a@(MonomialTerm u n) =
  case u of
    Sin u' -> contractSinPower n u'
    Cos u' -> contractCosPower n u'
    _ -> a
  where
    -- Calcular el binomio en los enteros y convertirlo a una expresión
    exprBinom n k = fromInteger $ choose n k

    contractCosPower :: Integer -> Expr -> Expr
    contractCosPower n x
      | n < 0 = cos x ** fromInteger n
      | even n = (exprBinom n (n `div` 2)) / 2 ^ n + 2 ^^ (1 - n) * sum [makeSumExpr j | j <- [0 .. (n `div` 2 - 1)]]
      | otherwise = 2 ^^ (1 - n) * sum [makeSumExpr j | j <- [0 .. n `div` 2]]
      where
        makeSumExpr j = exprBinom n j * cos (fromInteger (n - 2 * j) * x)

    contractSinPower :: Integer -> Expr -> Expr
    contractSinPower n x
      | n < 0 = (sin x) ** (fromInteger n)
      | even n = (-1 / 2) ^ n * exprBinom n (n `div` 2) + (-1) ^ (n `div` 2) * (2 ^^ (1 - n)) * sum [makeSumExpr cos j | j <- [0 .. (n `div` 2 - 1)]]
      | otherwise = (-1) ^ ((n - 1) `div` 2) * 2 ^^ (1 - n) * sum [makeSumExpr sin j | j <- [0 .. n `div` 2]]
      where
        makeSumExpr f j = (-1) ^ j * exprBinom n j * (f ((fromInteger (n - 2 * j)) * x))
contractTrigPower a = a

-- * Simplififcación de funciones trigonometricas

-- | Retorna la cantidad de funciones trigonometricas en la expresión
trigMeasure :: Expr -> Int
trigMeasure (Sin x) = 1 + trigMeasure x
trigMeasure (Cos x) = 1 + trigMeasure x
trigMeasure x = 1 + sum (map trigMeasure (operands x))


-- | Mapea una funcion al numerador y al denominador de una expresión
rationalMap :: (Expr -> Expr) -> Expr -> Expr
rationalMap f (Div n d) = (f n) / (f d)
rationalMap f x = f x

{-|
  Simplificacion de funciones trigonometricas

  === Ejemplos:
  >>> trigSimplify (sin(x)**2 + cos(x)**2)
  1

  >>> trigSimplify (sin(x)**4 - 2*cos(x)**2*sin(x)**2 + cos(x)**4)
  cos(4*x)/2+1/2
  
  >>> trigSimplify (sin(x)*tan(x)/sec(x))
  sin(x)^2

  >>> trigSimplify ((sin(x) + sin(3*x) + sin(5*x) + sin(7*x))/(cos(x) + cos(3*x) + cos(5*x) + cos(7*x)) - sin(4*x)/cos(4*x))
  0
-}
trigSimplify:: Expr -> Expr
trigSimplify x = let
                  x1 = rationalize x
                  x2 = rationalMap trigExpand x1
                  x3 = rationalMap trigContract x2
                 in
                  minimumBy (compare `on` trigMeasure) [x, x1, x2, x3]