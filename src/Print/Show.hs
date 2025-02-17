{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Print.Show (
    pretty,
) where

import Prelude hiding (reverse)

import Classes.Assumptions
import Expr
import Prettyprinter
import Simplification.PolyTools
import Structure

import Data.Foldable (toList)
import TwoList (sortBy, reverse)
import Data.Function
import Data.Char (toLower)

numberFactor :: Expr -> Expr
numberFactor n@(structure -> Number _) = n
numberFactor (structure -> Mul vs) = product $ fmap numberFactor vs
numberFactor (structure -> Pow u v) =
  let -- (a*b)**c = a**c * b**c, solo si a y b son positivos
      u' = numberFactor u
      u'' = u / u'
   in if true (isPositive u' &&& isPositive u'')
        then u' ** v
        else 1
numberFactor _ = 1

mulByNeg :: Expr -> Bool
mulByNeg = true . isNegative . numberFactor

toNumberSuperscript :: Expr -> String
toNumberSuperscript (structure -> Number n) = map toSuperscript $ show n
  where
    toSuperscript '-' = '⁻'
    toSuperscript '/' = 'ᐟ'
    toSuperscript '0' = '⁰'
    toSuperscript '1' = '¹'
    toSuperscript '2' = '²'
    toSuperscript '3' = '³'
    toSuperscript '4' = '⁴'
    toSuperscript '5' = '⁵'
    toSuperscript '6' = '⁶'
    toSuperscript '7' = '⁷'
    toSuperscript '8' = '⁸'
    toSuperscript '9' = '⁹'
    toSuperscript x = x
toNumberSuperscript x = show x

-- instance Pretty Expr where
instance Pretty Expr where
  pretty u =
    let n = numerator u
        d = denominator u
     in if d == 1
          then pretty' n
          else pretty' n <+> slash <+> pretty' d
    where
      pretty' (structure -> Number n) = viaShow n
      
      pretty' u'@(structure -> Add us) = 
        let
            vars = variables u'
            (v :|| vs) = reverse $ sortBy (compare `on` (multidegree vars)) us -- ordenar los monomios
        in 
            fillSep $ pretty v : (map addSigns $ toList vs)
        where
          addSigns y -- Agrega un operador + o - dependiendo del elemento
            | mulByNeg y = pretty "-" <+> pretty (negate y)
            | otherwise = pretty "+" <+> pretty y

      pretty' (structure -> Mul vs) = concatWith (surround (pretty "∙")) $ fmap mkPretty $ toList vs
        where
          -- Cerrar entre parentesis si no es entero positivo o es una suma
          mkPretty (structure -> Number s)
            | s < 0 || false (isInteger s) = parens $ viaShow s
          mkPretty v@(structure -> Add _) = parens $ pretty v
          mkPretty v = pretty v

      pretty' (structure -> Pow x n)
        | Number _ <- structure n = mkPretty x <> pretty (toNumberSuperscript n)
        where
            mkPretty u@(structure -> Symbol _) = pretty u
            mkPretty u@(structure -> Exp _) = parens $ pretty u
            mkPretty u@(structure -> Fun _ _) = pretty u
            mkPretty u = parens $ pretty u
      
      pretty' (structure -> Pow x y) = mkPretty x <+> pretty "^" <+> mkPretty y
        where
          mkPretty (structure -> Number s)
            | s < 0 || false (isInteger s) = parens $ viaShow s
          mkPretty v@(structure -> Add _) = parens $ pretty v
          mkPretty v@(structure -> Mul _) = parens $ pretty v
          mkPretty v@(structure -> Pow _ _) = parens $ pretty v
          mkPretty v = pretty v
      
      pretty' (structure -> Symbol s) = pretty s
      
      pretty' (structure -> Exp x) =
        let e = symbol "e"
         in pretty $ e ** x
      pretty' (structure -> Fun f (x :| [])) = pretty (lower f) <> parens (pretty x)
        where
            lower = map toLower
      pretty' v = viaShow v

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