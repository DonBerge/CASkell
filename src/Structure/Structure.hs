{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
    Module      : Structure
    Description : Manipulacion de la estructura de datos de las expresiones
-}
module Structure (
    SExpr(..),
    structure,
    TwoList(..),
    NonEmpty(..),
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
    freeOf,
    operands,
    construct,
    mapStructure
)
where

import TwoList (TwoList(..))

import qualified PExpr as P
import qualified Number as N

import Expr
import Classes.EvalSteps (EvalSteps(unEvalSteps))
import Symplify (simplifyFun)
import Data.List.NonEmpty (toList, NonEmpty(..))

data SExpr = Number N.Number | Symbol String | Add (TwoList Expr) | Mul (TwoList Expr) | Pow Expr Expr | Fun String (NonEmpty Expr) | Undefined String
  deriving (Eq, Show)

makeExpr :: P.PExpr -> Expr
makeExpr = return

structure :: Expr -> SExpr
structure x = case unEvalSteps x of
                (Left e, _) -> Undefined e
                (Right x', _) -> structure' x'

    where
        structure' (P.Number n) = Number n
        
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
construct (Undefined error) = fail error

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
pattern Integral u x = Fun "Integrate" (u :| [x])