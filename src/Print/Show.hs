{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Print.Show where

import Expr
import Structure
import Simplification.PolyTools

import Data.Function

-- import Data.List (intercalate, sortBy)

import TwoList (toList, intercalate, sortBy)
import Prettyprinter


-- |
-- Obtiene el grado de cada variable de la lista
multidegree :: [Expr] -> Expr -> [Integer]
multidegree vars p = map (degreeGPE p) vars


-- instance Pretty Expr where
instance Pretty Expr where
    pretty u = let
                n = numerator u
                d = denominator u
                in
                    if d == 1 then pretty' n
                    else pretty' n <+> pretty "/" <+> pretty' d
        where
            pretty' = undefined

{-
toDoc :: Expr -> String
toDoc u = let
                n = numerator u
                d = denominator u
             in
                if d == 1 then showExpr' n
                else showExpr' n ++ " / " ++ showExpr' d
    where
        showExpr' v@(structure -> Add vs) = 
            let
                vars = variables v
            in
                intercalate " + " $ fmap showExpr $ sortBy (compare `on` (multidegree vars)) $ vs
        -- showExpr' (structure -> Exp x)
        showExpr' v = show v
-}