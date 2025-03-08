{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
--    Module      : Expr.Structure
--    Description : Manipulacion de la estructura de datos de las expresiones
module Expr.Structure
  ( 
    TwoList (..),
    NonEmpty (..),

    -- * Sinonimos de patrones
    -- ** Patrones basicos
    pattern Number,
    pattern Symbol,
    pattern Add,
    pattern Mul,
    pattern Pow,
    pattern Fun,
    pattern Undefined,
    -- ** Patrones utiles
    pattern Neg,
    pattern MonomialTerm,
    pattern Sqrt,
    pattern Div,
    -- ** Simbolos y funciones conocidas
    -- *** Constantes
    pattern Pi,
    -- *** Exponencial y logaritmo
    pattern Exp,
    pattern Log,
    -- *** Trigonometricas
    pattern Sin,
    pattern Cos,
    -- *** Trigonometricas inversas
    pattern Asin,
    pattern Acos,
    pattern Atan,
    -- *** Derivadas e integrales
    pattern Derivative,
    pattern Integral,
    pattern DefiniteIntegral,

    -- * Operaciones sobre la estructura
    freeOf,
    operands,
    mapStructure,
  )
where

import Assumptions
import Classes.EvalResult (runEvalResult)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, toList)
import Data.Number (Number)
import Data.TwoList (TwoList (..))
import qualified Data.TwoList as TL (fromList, toList)
import Expr.ExprType
import PExpr (PExpr)
import qualified PExpr as P
import Expr.Simplify (simplifyFun, simplifyProduct, simplifySum)

makeExpr :: PExpr -> Expr
makeExpr = return

mkTwoOrMoreOperands :: [PExpr] -> Maybe (TwoList Expr)
mkTwoOrMoreOperands = fmap (fmap makeExpr) . TL.fromList

mkFunOperands :: [PExpr] -> Maybe (NonEmpty Expr)
mkFunOperands [] = Nothing
mkFunOperands xs = Just $ NE.fromList $ map return xs

-- * Funciones de matcheo

matchMulByNegative :: Expr -> Maybe Expr
matchMulByNegative e = case runEvalResult e of
  Right (P.Number n) | true (isNegative n) -> Just $ negate e
  Right (P.Mul (P.Number n : _)) | true (isNegative n) -> Just $ negate e
  _ -> Nothing
  --Right (P.Mul [P.Number n, x]) | true (isInteger n &&& n < 0) -> Just (return x)
  --_ -> Nothing

matchUnaryFun :: String -> Expr -> Maybe Expr
matchUnaryFun s e = case runEvalResult e of
  Right (P.Fun s' [x]) | s == s' -> Just (return x)
  _ -> Nothing

matchAnyarityFun :: String -> Expr -> Maybe (NonEmpty Expr)
matchAnyarityFun s e = case runEvalResult e of
  Right (P.Fun s' xs) | s == s' -> mkFunOperands xs
  _ -> Nothing

matchMonomialTerm :: Expr -> Maybe (Expr, Integer)
matchMonomialTerm e = case runEvalResult e of
  Right (P.Pow x (P.Number n)) | true (isInteger n &&& n > 1) -> Just (return x, toInteger n)
  _ -> Nothing

matchFun :: Expr -> Maybe (String, NonEmpty Expr)
matchFun e = case runEvalResult e of
  Right (P.Fun s (x : xs)) -> Just (s, fmap return (x :| xs))
  _ -> Nothing

pattern Number :: Number -> Expr
pattern Number n <- (runEvalResult -> Right (P.Number n))

pattern Symbol :: String -> Expr
pattern Symbol s <- (runEvalResult -> Right (P.Symbol s))

pattern Add :: TwoList Expr -> Expr
pattern Add xs <- (runEvalResult -> Right (P.Add (mkTwoOrMoreOperands -> Just xs)))

pattern Mul :: TwoList Expr -> Expr
pattern Mul xs <- (runEvalResult -> Right (P.Mul (mkTwoOrMoreOperands -> Just xs)))

pattern Pow :: Expr -> Expr -> Expr
pattern Pow x y <- (runEvalResult -> Right (P.Pow (makeExpr -> x) (makeExpr -> y)))

pattern Fun :: String -> NonEmpty Expr -> Expr
pattern Fun s xs <- (matchFun -> Just (s, xs))

pattern Undefined :: String -> Expr
pattern Undefined e <- (runEvalResult -> Left e)

pattern Pi :: Expr
pattern Pi <- Symbol "Pi"

pattern Exp :: Expr -> Expr
pattern Exp x <- Fun "Exp" (x :| [])

pattern Log :: Expr -> Expr
pattern Log x <- (matchUnaryFun "Log" -> Just x)

pattern Sin :: Expr -> Expr
pattern Sin x <- (matchUnaryFun "Sin" -> Just x)

pattern Cos :: Expr -> Expr
pattern Cos x <- (matchUnaryFun "Cos" -> Just x)

pattern Asin :: Expr -> Expr
pattern Asin x <- (matchUnaryFun "Asin" -> Just x)

pattern Acos :: Expr -> Expr
pattern Acos x <- (matchUnaryFun "Acos" -> Just x)

pattern Atan :: Expr -> Expr
pattern Atan x <- (matchUnaryFun "Atan" -> Just x)

pattern Derivative :: Expr -> Expr -> Expr
pattern Derivative u x <- (matchAnyarityFun "Derivate" -> Just (u :| [x]))

pattern Integral :: Expr -> Expr -> Expr
pattern Integral u x <- (matchAnyarityFun "Integral" -> Just (u :| [x]))

pattern DefiniteIntegral :: Expr -> Expr -> Expr -> Expr -> Expr
pattern DefiniteIntegral u x a b <- (matchAnyarityFun "Definite_Integral" -> Just (u :| [x, a, b]))

pattern MonomialTerm :: Expr -> Integer -> Expr
pattern MonomialTerm x n <- (matchMonomialTerm -> Just (x, n))

pattern Neg :: Expr -> Expr
pattern Neg x <- (matchMulByNegative -> Just x)

pattern Sqrt :: Expr -> Expr
pattern Sqrt x <- Pow x (Number 0.5)

pattern Div :: Expr -> Expr -> Expr
pattern Div n d <- ((\x -> (numerator x, denominator x)) -> (n, d))

-- * Manipulación de la estructura de las expresiones

freeOf :: Expr -> Expr -> Bool
freeOf u t
  | u == t = False
freeOf (Symbol _) _ = True
freeOf (Number _) _ = True
freeOf u t = all (`freeOf` t) $ operands u

operands :: Expr -> [Expr]
operands (Add (x :|| y :| xs)) = x : y : xs
operands (Mul (x :|| y :| xs)) = x : y : xs
operands (Pow b e) = [b, e]
operands (Fun _ xs) = NE.toList xs
operands _ = []

mapStructure :: (Expr -> Expr) -> Expr -> Expr
mapStructure f (Add xs) = mapM f xs >>= simplifySum . TL.toList
mapStructure f (Mul xs) = mapM f xs >>= simplifyProduct . TL.toList
mapStructure f (Pow b e) = (f b) ** (f e)
mapStructure f (Fun s xs) = mapM f xs >>= simplifyFun . P.Fun s . NE.toList
mapStructure _ x = x