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
    NonEmpty(..)
)
where

import TwoList

import qualified PExpr as P
import qualified Number as N

import Expr
import Classes.EvalSteps (EvalSteps(unEvalSteps))

data SExpr = Number N.Number | Symbol String | Add (TwoList Expr) | Mul (TwoList Expr) | Pow Expr Expr | Fun String (NonEmpty Expr) | Undefined
  deriving (Eq, Show)

makeExpr :: P.PExpr -> Expr
makeExpr = return

structure :: Expr -> SExpr
structure x = case unEvalSteps x of
                (Left _, _) -> Undefined
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