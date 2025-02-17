{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Simplification.Trigonometric where

import Calculus.Integrate (substitute)
import Classes.Assumptions
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.Matrix as Matrix
import Expr
import Math.Combinatorics.Exact.Binomial (choose)
import Simplification.Algebraic (expandMainOp)
import qualified Simplification.Algebraic as Algebraic
import Structure

-- * Substitucion de funciones trigonometricas

-- |
--    Reemplaza las ocurrencias de 'tan', 'cot', 'sec' y 'csc' por sus equivalentes en seno y coseno.
--
--    > trigSubstitute (tan x) = sin x / cos xç
--    > trigSubstitute (cot x) = cos x / sin x
--    > trigSubstitute (cosec x + sec y) = 1 / sin x + 1 / cos y
trigSubstitute :: Expr -> Expr
trigSubstitute = mapStructure trigSubstitute . trigSubstitute'
  where
    trigSubstitute' (structure -> Tan x) = sin x / cos x
    trigSubstitute' (structure -> Cot x) = cos x / sin x
    trigSubstitute' (structure -> Sec x) = 1 / cos x
    trigSubstitute' (structure -> Csc x) = 1 / sin x
    trigSubstitute' x = x

-- * Expansion de expresiones trigonometricas

-- |
--    Convierte expresiones en su forma trigonometrica expandida.
--
--    Una expresion esta en forma trigonometrica expandida si cada argumento de un seno o coseno cumple que:
--
--        1. No es una suma;
--
--        2. No es un producto con un operando que es un entero.
trigExpand :: Expr -> Expr
trigExpand = trigExpand' . mapStructure trigExpand
  where
    trigExpand' (structure -> Sin x) = fst $ expandTrigRules x
    trigExpand' (structure -> Cos x) = snd $ expandTrigRules x
    trigExpand' x = x

-- |
--  Dada una expresión @u@, 'expandTrigRules' calcula la forma trigonometrica expandida de @sin u@ y @cos u@.
--
--  La expansión se calcula utilizando las siguientes propiedades
--
--  - Si @u=v+w@ entonces la forma expandida se calcula usando las siguientes propiedades
--    \[\sin(v+w) = \sin(v)\cos(w)+\cos(v)\sin(w)\]
--    \[\cos(v+w) = \cos(v)\cos(w)-\sin(v)\sin(w)\]
--
--  - Si @u=n*v@ con @n@ un entero, entonces la forma expandida se calcula utilizando 'multipleAngleCos' y 'multipleAngleSin'
expandTrigRules :: Expr -> (Expr, Expr)
-- u = v + w
expandTrigRules u@(structure -> Add (x :|| _)) =
  let f = expandTrigRules x
      r = expandTrigRules (u - x)
      s = fst f * snd r + snd f * fst r
      c = snd f * snd r - fst f * fst r
   in (s, c)
expandTrigRules u@(structure -> Mul (f :|| _))
  | Number f' <- structure f,
    true (isInteger f') -- u = n * v
    =
      let f'' = toInteger f'
          u' = u / f
       in (multipleAngleSin f'' u', multipleAngleCos f'' u')
expandTrigRules u = (sin u, cos u)

-- |
--  Expansión de \(\cos(n \cdot x)\), utilizando los polinomios de Chebyshev de primera clase.
multipleAngleCos :: Integer -> Expr -> Expr
multipleAngleCos n u@(structure -> Add _) = trigExpand $ cos $ Algebraic.expandMainOp $ fromInteger n * u
multipleAngleCos 0 _ = 1
multipleAngleCos 1 x = cos x
multipleAngleCos n x
  | n < 0 = multipleAngleCos (-n) x
  | otherwise =
      let x' = symbol "_"
          f = cheby1 x' n
       in substitute f x' (cos x)

-- |
--  Expansión de \(\sin(n \cdot x)\), utilizando los polinomios de Chebyshev de segunda clase.
multipleAngleSin :: Integer -> Expr -> Expr
multipleAngleSin n u@(structure -> Add _) = trigExpand $ sin $ Algebraic.expandMainOp $ fromInteger n * u
multipleAngleSin 0 _ = 0
multipleAngleSin 1 x = sin x
multipleAngleSin n x
  | n < 0 = -(multipleAngleSin (-n) x)
  | otherwise =
      let x' = symbol "_"
          f = cheby2 x' (n - 1)
       in Algebraic.expand $ (substitute f x' (cos x)) * sin x

-- |
--    Generación de polinomios de Chebyshev de primera clase.
--
--    Los polinomios de Chebyshev de primera clase cumplen la siguiente propiedad:
--        \[T_n(\cos x) = \cos(n \cdot x)\]
--
--    Por lo que son utiles para realizar la expansión de \(\cos(n \cdot x)\)
cheby1 :: (Integral b) => Expr -> b -> Expr
cheby1 _ 0 = 1
cheby1 x 1 = x
cheby1 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2 * x, -1], [1, 0]] ^ (n - 1)) * Matrix.fromLists [[x], [1]]

-- |
--    Generación de polinomios de Chebyshev de segunda clase.
--
--    Los polinomios de Chebyshev de segunda clase cumplen la siguiente propiedad:
--        \[U_n(\cos x)\sin x = {\sin((n+1)x)}\]
--
--    Por lo que son utiles para realizar la expansión de \(\sin(n \cdot x)\)
cheby2 :: (Integral b) => Expr -> b -> Expr
cheby2 _ 0 = 1
cheby2 x 1 = 2 * x
cheby2 x n = Algebraic.expand $ Matrix.getElem 1 1 $ (Matrix.fromLists [[2 * x, -1], [1, 0]] ^ n) * Matrix.fromLists [[1], [0]]

-- * Contracion de expresiones trigonometricas

-- |
--    Conversión de expresiones trigonometricas a su forma trigonometrica contraida.
--
--    Una expresion esta en forma trigonometrica contraida si satisface que:
--
--        1. Cualquier producto en la expresión tiene como mucho un operando que es un seno o coseno;
--        2. Una potencia con exponente entero positivo no tiene como base a un seno o coseno;
--        3. Cualquier subexpresión esta en forma expandida.
contractTrig :: Expr -> Expr
contractTrig = mapStructure contractTrig . contractTrig'
  where
    contractTrig' v@(structure -> Pow _ _) = contractTrigRules $ expandMainOp v
    contractTrig' v@(structure -> Mul _) = contractTrigRules $ expandMainOp v
    contractTrig' v = v

contractTrigRules :: Expr -> Expr
contractTrigRules v@(structure -> Pow _ _) = contractTrigPower v
contractTrigRules v@(structure -> Mul _) =
  let (c, d) = separateSinCos v
   in if d == 1
        then v
        else case structure d of
          Sin _ -> v
          Cos _ -> v
          Pow _ _ -> expandMainOp (c * contractTrigPower d)
          Mul ds -> expandMainOp (c * contractTrigProduct ds)
          _ -> fail "Contract trig rules: Unreachable case"
contractTrigRules (structure -> Add us) = sum $ fmap trigRules us
  where
    trigRules v@(structure -> Pow _ _) = contractTrigRules v
    trigRules v@(structure -> Mul _) = contractTrigRules v
    trigRules v = v
contractTrigRules v = v

-- |
--    Verifica que la expresión dada es un seno, coseno o una potencia entera de un seno o coseno
isSinOrCos :: Expr -> Bool
isSinOrCos (structure -> Sin _) = True
isSinOrCos (structure -> Cos _) = True
isSinOrCos (structure -> Pow v w)
  | true $ isSinOrCos v &&& isInteger w = True
isSinOrCos _ = False

-- |
--    Dada una expresión @u@, 'separateSinCos' separa los senos y cosenos de los demás términos.
separateSinCos :: Expr -> (Expr, Expr)
separateSinCos (structure -> Mul (u :|| v :| us)) = bimap product product $ separateSinCos' (u : v : us)
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
  | Pow _ _ <- structure a = contractTrigRules $ (contractTrigPower a) * b
  | Pow _ _ <- structure b = contractTrigRules $ a * contractTrigPower b
  | otherwise = case (structure a, structure b) of -- a y b son funciones trigonometricas
      (Sin p, Sin q) -> cos (p - q) / 2 - cos (p + q) / 2
      (Cos p, Cos q) -> cos (p - q) / 2 + cos (p + q) / 2
      (Sin p, Cos q) -> sin (p + q) / 2 + sin (p - q) / 2
      (Cos p, Sin q) -> sin (p + q) / 2 - sin (q - p) / 2
      _ -> a * b -- este caso no deberia darse
contractTrigProduct (a :|| b :| c : cs) = contractTrigRules $ a * contractTrigProduct (b :|| c :| cs)

-- |
--    Contraccion de potencias de funciones trigonometricas.
contractTrigPower :: Expr -> Expr
contractTrigPower a@(structure -> MonomialTerm u n) =
  case structure u of
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