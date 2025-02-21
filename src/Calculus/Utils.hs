{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Calculus.Utils where

import Expr
import Simplification.PolyTools (variables)
import Structure

-- | Verifica si una expresión dada es una variable
notAVariable :: Expr -> Bool
notAVariable (structure -> Pi) = True
notAVariable (structure -> Symbol _) = False
notAVariable _ = True

-- |
--    Sustitucion de expresiones dentro de otra expresion
substitute :: Expr -> Expr -> Expr -> Expr
substitute u t r
  | u == t = r
  | otherwise = mapStructure (\u' -> substitute u' t r) u

-- Obtiene un nombre de variable de integración que no este en la expresion
getNewVariable :: Expr -> Expr -> Expr
getNewVariable u (structure -> Symbol x) = getNewVariable' x
  where
    vars = variables u
    getNewVariable' x =
      let symbol_x = construct $ Symbol $ x
       in if symbol_x `elem` vars
            then getNewVariable' ('_' : x)
            else symbol_x
getNewVariable _ _ = undefinedExpr "La variable debe ser un simbolo"