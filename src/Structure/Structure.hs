{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
--    Module      : Structure
--    Description : Manipulacion de la estructura de datos de las expresiones
module Structure (
                     -- SExpr(..),
                     structure,
                     TwoList(..),
                     NonEmpty(..),

                     pattern Number,
                     pattern Symbol,
                     pattern Add,
                     pattern Mul,
                     pattern Pow,
                     pattern Fun,
                     pattern Undefined,


                     pattern Pi,
                     pattern Exp,
                     pattern Log,
                     pattern Sin,
                     pattern Cos,
                     pattern Tan,
                     pattern Cot,
                     pattern Sec,
                     pattern Csc,
                     pattern Asin,
                     pattern Acos,
                     pattern Atan,
                     pattern Asinh,
                     pattern Acosh,
                     pattern Atanh,
                     pattern Sinh,
                     pattern Cosh,
                     pattern Tanh,
                     pattern Derivative,
                     pattern Integral,
                     pattern DefiniteIntegral,
                     pattern MonomialTerm,
                     freeOf,
                     operands,
                     --construct,
                     mapStructure,
                     --showStruct
                 ) where

import Assumptions
import Classes.EvalSteps (runEvalSteps)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, toList)

import Data.Number (Number)
import Data.TwoList (TwoList (..))
import qualified Data.TwoList as TL (fromList, toList)
import Expr
import PExpr (PExpr)
import qualified PExpr as P
import Symplify (simplifyFun, simplifySum, simplifyProduct)

structure :: a -> a
structure = id

makeExpr :: PExpr -> Expr
makeExpr = return

mkTwoOrMoreOperands :: [PExpr] -> Maybe (TwoList Expr)
mkTwoOrMoreOperands = fmap (fmap makeExpr) . TL.fromList

mkFunOperands :: [PExpr] -> Maybe (NonEmpty Expr)
mkFunOperands [] = Nothing
mkFunOperands xs = Just $ NE.fromList $ map return xs

-- * Funciones de matcheo

matchUnaryFun :: String -> Expr -> Maybe Expr
matchUnaryFun s e = case runEvalSteps e of
  Right (P.Fun s' [x]) | s == s' -> Just (return x)
  _ -> Nothing

matchAnyarityFun :: String -> Expr -> Maybe (NonEmpty Expr)
matchAnyarityFun s e = case runEvalSteps e of
  Right (P.Fun s' xs) | s == s' -> mkFunOperands xs
  _ -> Nothing

matchMonomialTerm :: Expr -> Maybe (Expr, Integer)
matchMonomialTerm e = case runEvalSteps e of
  Right (P.Pow x (P.Number n)) | true (isInteger n &&& n > 1) -> Just (return x, toInteger n)
  _ -> Nothing

matchFun :: Expr -> Maybe (String, NonEmpty Expr)
matchFun e = case runEvalSteps e of
  Right (P.Fun s (x : xs)) -> Just (s, fmap return (x :| xs))
  _ -> Nothing

-- * Sinonimos de patrones

-- ** Patrones de expresiones

pattern Number :: Number -> Expr
pattern Number n <- (runEvalSteps -> Right (P.Number n))

pattern Symbol :: String -> Expr
pattern Symbol s <- (runEvalSteps -> Right (P.Symbol s))

pattern Add :: TwoList Expr -> Expr
pattern Add xs <- (runEvalSteps -> Right (P.Add (mkTwoOrMoreOperands -> Just xs)))

pattern Mul :: TwoList Expr -> Expr
pattern Mul xs <- (runEvalSteps -> Right (P.Mul (mkTwoOrMoreOperands -> Just xs)))

pattern Pow :: Expr -> Expr -> Expr
pattern Pow x y <- (runEvalSteps -> Right (P.Pow (makeExpr -> x) (makeExpr -> y)))

pattern Fun :: String -> NonEmpty Expr -> Expr
pattern Fun s xs <- (matchFun -> Just (s, xs))

pattern Undefined :: String -> Expr
pattern Undefined e <- (runEvalSteps -> Left e)

-- ** Patrones de funciones

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

pattern Tan :: Expr -> Expr
pattern Tan x <- (matchUnaryFun "Tan" -> Just x)

pattern Cot :: Expr -> Expr
pattern Cot x <- (matchUnaryFun "Cot" -> Just x)

pattern Sec :: Expr -> Expr
pattern Sec x <- (matchUnaryFun "Sec" -> Just x)

pattern Csc :: Expr -> Expr
pattern Csc x <- (matchUnaryFun "Csc" -> Just x)

pattern Asin :: Expr -> Expr
pattern Asin x <- (matchUnaryFun "Asin" -> Just x)

pattern Acos :: Expr -> Expr
pattern Acos x <- (matchUnaryFun "Acos" -> Just x)

pattern Atan :: Expr -> Expr
pattern Atan x <- (matchUnaryFun "Atan" -> Just x)

pattern Asinh :: Expr -> Expr
pattern Asinh x <- (matchUnaryFun "Asinh" -> Just x)

pattern Acosh :: Expr -> Expr
pattern Acosh x <- (matchUnaryFun "Acosh" -> Just x)

pattern Atanh :: Expr -> Expr
pattern Atanh x <- (matchUnaryFun "Atanh" -> Just x)

pattern Sinh :: Expr -> Expr
pattern Sinh x <- (matchUnaryFun "Sinh" -> Just x)

pattern Cosh :: Expr -> Expr
pattern Cosh x <- (matchUnaryFun "Cosh" -> Just x)

pattern Tanh :: Expr -> Expr
pattern Tanh x <- (matchUnaryFun "Tanh" -> Just x)

pattern Derivative :: Expr -> Expr -> Expr
pattern Derivative u x <- (matchAnyarityFun "Derivate" -> Just (u :| [x]))

pattern Integral :: Expr -> Expr -> Expr
pattern Integral u x <- (matchAnyarityFun "Integral" -> Just (u :| [x]))

pattern DefiniteIntegral :: Expr -> Expr -> Expr -> Expr -> Expr
pattern DefiniteIntegral u x a b <- (matchAnyarityFun "Definite_Integral" -> Just (u :| [x, a, b]))

pattern MonomialTerm :: Expr -> Integer -> Expr
pattern MonomialTerm x n <- (matchMonomialTerm -> Just (x, n))

-- * Manipulación de la estructura de las expresiones

freeOf :: Expr -> Expr -> Bool
freeOf u t
    | u == t = False
freeOf (Symbol _) _ = True
freeOf (Number _) _ = True
freeOf u t = all (`freeOf` t) $ operands u

operands :: Expr -> [Expr]
operands (Add (x :|| y :| xs)) = x:y:xs
operands (Mul (x :|| y :| xs)) = x:y:xs
operands (Pow b e) = [b, e]
operands (Fun _ xs) = NE.toList xs
operands _ = []

mapStructure :: (Expr -> Expr) -> Expr -> Expr
mapStructure f (Add xs) = mapM f xs >>= simplifySum . TL.toList
mapStructure f (Mul xs) = mapM f xs >>= simplifyProduct . TL.toList
mapStructure f (Pow b e) = (f b) ** (f e)
mapStructure f (Fun s xs) = mapM f xs >>= simplifyFun . P.Fun s . NE.toList
mapStructure _ x = x


{-
data SExpr = Number Number | Symbol String | Add (TwoList Expr) | Mul (TwoList Expr) | Pow Expr Expr | Fun String (NonEmpty Expr) | Undefined String
  deriving (Eq, Show)

structure :: Expr -> SExpr
structure x = case runEvalSteps x of
                Left e -> Undefined e
                Right x' -> structure' x'

    where
        structure' (P.Number n) = Number n

        structure' (P.SymbolWithAssumptions s _) = Symbol s

        structure' (P.Add []) = Number 0
        structure' (P.Add [x]) = structure' x
        structure' (P.Add xs) = let
                                  (x:y:xs') = map makeExpr xs
                                in Add (x :|| y :| xs')

        structure' (P.Mul []) = Number 1
        structure' (P.Mul [x]) = structure' x
        structure' (P.Mul xs) = let
                                  (x:y:xs') = map makeExpr xs
                                in Mul (x :|| y :| xs')

        structure' (P.Pow x y) = Pow (makeExpr x) (makeExpr y)

        structure' (P.Fun s []) = Symbol s
        structure' (P.Fun s (x:xs)) = Fun s (makeExpr x :| (map makeExpr xs))

construct :: SExpr -> Expr
construct (Number n) = return $ P.Number n
construct (Symbol s) = return $ P.Fun s []
construct (Add xs) = sum xs
construct (Mul xs) = product xs
construct (Pow b e) = b ** e
construct (Fun s xs) = sequence xs >>= simplifyFun . P.Fun s . toList
construct (Undefined error) = undefinedExpr error

freeOf :: Expr -> Expr -> Bool
freeOf u t
    | u == t = False
freeOf (structure -> Symbol _) _ = True
freeOf (structure -> Number _) _ = True
freeOf u t = all (`freeOf` t) $ operands u

operands :: Expr -> [Expr]
operands (structure -> Add (x :|| y :| xs)) = x:y:xs
operands (structure -> Mul (x :|| y :| xs)) = x:y:xs
operands (structure -> Pow b e) = [b, e]
operands (structure -> Fun _ xs) = toList xs
operands _ = []

mapStructure :: (Expr -> Expr) -> Expr -> Expr
mapStructure f (structure -> Add xs) = construct $ Add $ fmap f xs
mapStructure f (structure -> Mul xs) = construct $ Mul $ fmap f xs
mapStructure f (structure -> Pow b e) = construct $ Pow (f b) (f e)
mapStructure f (structure -> Fun s xs) = construct $ Fun s $ fmap f xs
mapStructure _ x = x

pattern Pi :: SExpr
pattern Pi = Symbol "Pi"

pattern Exp :: Expr -> SExpr
pattern Exp x = Fun "Exp" (x :| [])

pattern Log :: Expr -> SExpr
pattern Log x = Fun "Log" (x :| [])

pattern Sin :: Expr -> SExpr
pattern Sin x = Fun "Sin" (x :| [])

pattern Cos :: Expr -> SExpr
pattern Cos x = Fun "Cos" (x :| [])

pattern Tan :: Expr -> SExpr
pattern Tan x = Fun "Tan" (x :| [])

pattern Cot :: Expr -> SExpr
pattern Cot x = Fun "Cot" (x :| [])

pattern Sec :: Expr -> SExpr
pattern Sec x = Fun "Sec" (x :| [])

pattern Csc :: Expr -> SExpr
pattern Csc x = Fun "Csc" (x :| [])

pattern Asin :: Expr -> SExpr
pattern Asin x = Fun "Asin" (x :| [])

pattern Acos :: Expr -> SExpr
pattern Acos x = Fun "Acos" (x :| [])

pattern Atan :: Expr -> SExpr
pattern Atan x = Fun "Atan" (x :| [])

pattern Asinh :: Expr -> SExpr
pattern Asinh x = Fun "Asinh" (x :| [])

pattern Acosh :: Expr -> SExpr
pattern Acosh x = Fun "Acosh" (x :| [])

pattern Atanh :: Expr -> SExpr
pattern Atanh x = Fun "Atanh" (x :| [])

pattern Sinh :: Expr -> SExpr
pattern Sinh x = Fun "Sinh" (x :| [])

pattern Cosh :: Expr -> SExpr
pattern Cosh x = Fun "Cosh" (x :| [])

pattern Tanh :: Expr -> SExpr
pattern Tanh x = Fun "Tanh" (x :| [])

pattern Derivative :: Expr -> Expr -> SExpr
pattern Derivative u x = Fun "Derivate" (u :| [x])

pattern Integral :: Expr -> Expr -> SExpr
pattern Integral u x = Fun "Integral" (u :| [x])

pattern DefiniteIntegral :: Expr -> Expr -> Expr -> Expr -> SExpr
pattern DefiniteIntegral u x a b = Fun "Definite_Integral" (u:| [x,a,b])

-- |
-- PatternSynoym for a monomial term
pattern MonomialTerm :: Expr -> Integer -> SExpr
pattern MonomialTerm x n <- Pow x (positiveIntegerDegree -> Just n)

positiveIntegerDegree :: Expr -> Maybe Integer -- Se asume que el argumento es un exponente, por autosimplificación no puede ser 0
positiveIntegerDegree (structure -> Number n)
  | true (isInteger n &&& n > 1) = Just $ toInteger n
positiveIntegerDegree _ = Nothing

showStruct :: Expr -> String
showStruct (structure -> Number n) = "Number " ++ show n
showStruct (structure -> Symbol s) = "Symbol " ++ s
showStruct (structure -> Add xs) = "Add " ++ show xs
showStruct (structure -> Mul xs) = "Mul " ++ show xs
showStruct (structure -> Pow b e) = "Pow " ++ show b ++ " " ++ show e
showStruct (structure -> Fun s xs) = "Fun " ++ s ++ " " ++ show xs
showStruct (structure -> Undefined e) = "Undefined " ++ e
-}