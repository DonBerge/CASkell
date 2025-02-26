{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PrettyPrint (
    pretty,
) where

import Prelude hiding (reverse)

import Assumptions
import Expr
import Prettyprinter
import Simplification.PolyTools
import Structure

import Data.Foldable (toList)
import Data.TwoList (sortBy, reverse)
import Data.Function
import Data.Char (toLower, toUpper)

numberFactor :: Expr -> Expr
numberFactor n@(Number _) = n
numberFactor (Mul vs) = product $ fmap numberFactor vs
numberFactor (Pow u v) =
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
toNumberSuperscript (Number n) = map toSuperscript $ show n
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
          else prettyDivision n d --pretty' n <+> slash <+> pretty' d
    where
      prettyDivision n d = mkPretty n <> slash <> mkPretty d
        where
          mkPretty u@(Add _) = parens $ pretty' u
          mkPretty u@(Mul _) = parens $ pretty' u
          mkPretty u = pretty' u


      pretty' v
        | mulByNeg v = pretty "-" <> mkPretty (negate v)
        where
          mkPretty u@(Add _) = parens $ pretty u
          mkPretty u = pretty u

      pretty' (Number n) = viaShow n
      
      pretty' u'@(Add us) = 
        let
            vars = variables u'
            (v :|| vs) = reverse $ sortBy (compare `on` (multidegree vars)) us -- ordenar los monomios
        in 
            fillSep $ pretty v : (map addSigns $ toList vs)
        where
          addSigns y -- Agrega un operador + o - dependiendo del elemento
            | mulByNeg y = pretty "-" <+> pretty (negate y)
            | otherwise = pretty "+" <+> pretty y

      pretty' (Mul vs) = concatWith (surround (pretty "∙")) $ fmap mkPretty $ toList vs
        where
          -- Cerrar entre parentesis si no es entero positivo o es una suma
          mkPretty v@(Add _) = parens $ pretty v
          mkPretty v = pretty v

      pretty' (Pow x n)
        | Number _ <- structure n = mkPretty x <> pretty (toNumberSuperscript n)
        where
            mkPretty u@(Symbol _) = pretty u
            mkPretty u@(Exp _) = parens $ pretty u
            mkPretty u@(Fun _ _) = pretty u
            mkPretty u = parens $ pretty u
      
      pretty' (Pow x y) = mkPretty x <> pretty "^" <> mkPretty y
        where
          mkPretty v@(Add _) = parens $ pretty v
          mkPretty v@(Mul _) = parens $ pretty v
          mkPretty v@(Pow _ _) = parens $ pretty v
          mkPretty v = pretty v
      
      pretty' (Symbol s) = pretty s
      
      pretty' (Exp x) =
        let e = symbol "e"
         in pretty $ e ** x
      pretty' (Fun f (x :| [])) = pretty (camelCase f) <> parens (pretty x)
        where
            camelCase (words -> []) = ""
            camelCase (words -> (y:ys)) = lower y ++ concatMap capitalize ys

            capitalize [] = ""
            capitalize (y:ys) = toUpper y : lower ys

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
        showExpr' v@(Add vs) =
            let
                vars = variables v
            in
                intercalate " + " $ fmap showExpr $ sortBy (compare `on` (multidegree vars)) $ vs
        -- showExpr' (Exp x)
        showExpr' v = show v
-}