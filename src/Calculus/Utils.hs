{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Calculus.Utils where

import Expr

-- | Verifica si una expresión dada es una variable
notAVariable :: Expr -> Bool
notAVariable Pi = True
notAVariable (Symbol _) = False
notAVariable _ = True

-- |
--    Sustitucion de expresiones dentro de otra expresion
substitute :: Expr -> Expr -> Expr -> Expr
substitute u t r
  | u == t = r
  | otherwise = mapStructure (\u' -> substitute u' t r) u

-- | Dada una expresión \(u\) y un simbolo \(x\), obtiene un nuevo simbolo \(x\) que no este en \(u\)
getNewVariable :: Expr -> Expr -> Expr
getNewVariable u (Symbol x) = getNewVariable' x
  where
    getNewVariable' x =
      let symbol_x = symbol x
       in if u `freeOf` symbol_x
            then symbol_x 
            else getNewVariable' ('_' : x)
getNewVariable _ _ = undefinedExpr "La variable debe ser un simbolo"