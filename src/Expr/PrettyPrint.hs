{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--  Module      : PrettyPrint
--  Description : Pretty printing de expresiones
--
--  La gramatica para el parseo de expresiones es similar a la siguiente:
--
--  @
--  Expression : Expression '+' Expression
--             | Expression '-' Expression
--             | Expression '*' Expression
--             | Expression '^' Expression
--             | '(' Expression ')'
--             | number
--             | symbol
--             | symbol Arguments
--
--  Arguments : '(' CommaArguments ')'
--
--  CommaArguments : Expression
--                 | Expression ',' CommaArguments
--  @
--
--  Esta gramatica puede desambiguarse en la siguiente gramatica:
--
--  @
--  Expression : Term '+' Expression
--             | Term '-' Expression
--             | Term
--
--  Term : Factor '*' Term
--       | Factor
--
--  Factor : Base '^' Factor
--         | Base
--
--  Base : '(' Expression ')'
--       | number
--       | symbol
--       | symbol '(' CommaArguments ')'
--
--  CommaArguments : Expression ',' CommaArguments
--                 | Expression
--  @
--
--  Esta ultima gramatica es la que se utiliza en el PrettyPrint de las expresiones.
module Expr.PrettyPrint
  ( pretty,
    latex
  )
where

import Data.Foldable (toList)
import Data.Function
import Data.TwoList (sortBy)

import Expr.ExprType
import Expr.Structure

import Prettyprinter
import Expr.PolyTools
import Prelude hiding (reverse)
import Data.Number (printAsFraction)

-- * Pretty printing de los simbolos no terminales

prettyExpression :: Expr -> Doc ann
prettyExpression (Undefined e) = pretty "Undefined:" <+> pretty e
prettyExpression u@(Add us) =
  let vars =  variables u
      (v :|| vs) = sortBy (flip compare `on` multidegree vars) us -- ordenar los monomios segun el multigrado
   in prettyTerm v <> hcat (map addSigns (toList vs))
  where
    -- Agrega un operador + o - dependiendo del elemento
    addSigns (Neg y) = pretty "-" <> prettyTerm y
    addSigns y = pretty "+" <> prettyTerm y
prettyExpression u = prettyTerm u

prettyTerm :: Expr -> Doc ann
prettyTerm u@(Exp _) = prettyFactor u -- Evita separar denominador y numerador de expresiones como e^(-x) 
prettyTerm u
  | d == 1 = case u of
              Neg u -> pretty "-" <> prettyMul u
              _ -> prettyMul u
  | otherwise = prettyTerm n <> slash <> prettyFactor d
  where
    prettyMul (Mul us) = concatWith (surround (pretty "*")) $ fmap prettyFactor us
    prettyMul u = prettyFactor u

    n = numerator u
    d = denominator u

prettyFactor :: Expr -> Doc ann
prettyFactor (Sqrt x) = pretty "√" <> prettyBase x
prettyFactor (Pow x y) = prettyBase x <> pretty "^" <> prettyBase y -- El exponente se imprime usando 'prettyBase' para desambiguar expresiones como x**y**z
prettyFactor (Exp 1) = pretty "e"
prettyFactor (Exp x) = pretty "e" <> pretty "^" <> prettyBase x
prettyFactor u = prettyBase u

prettyBase :: Expr -> Doc ann
prettyBase (Number n)
  | printAsFraction n = parens $ viaShow n
  | otherwise = viaShow n
prettyBase Pi = pretty "π"
prettyBase (Symbol s) = pretty s
prettyBase u@(Exp _) = parens $ prettyFactor u -- Tratar e^x como un factor
prettyBase (Fun name us) = pretty name <> parens (concatWith (surround comma) (fmap prettyExpression us))
prettyBase u = parens $ prettyExpression u

-- * Funcion de pretty printing

instance Pretty Expr where
  pretty = prettyExpression

instance Show Expr where
  show = show . pretty

-- Latex

latexExpression :: Expr -> [Char]
latexExpression (Undefined e) = "Undefined: " <>  e
latexExpression u@(Add us) =
  let vars =  variables u
      (v :|| vs) = sortBy (flip compare `on` multidegree vars) us -- ordenar los monomios segun el multigrado
   in latexTerm v <> concatMap addSigns vs
  where
    -- Agrega un operador + o - dependiendo del elemento
    addSigns (Neg y) = "-" <> latexTerm y
    addSigns y = "+" <> latexTerm y
latexExpression u = latexTerm u

latexTerm :: Expr -> [Char]
latexTerm u@(Exp _) = latexFactor u -- Evita separar denominador y numerador de expresiones como e^(-x) 
latexTerm u
  | d == 1 = case u of
              Neg u -> "-" <> latexMul u
              _ -> latexMul u
  | otherwise = "\\frac{" <> latexTerm n <> "}{" <> latexFactor d <> "}"
  where
    latexMul (Mul us) = concatWith (surround "*") $ fmap latexFactor us
    latexMul u = latexFactor u

    surround x l r = l <> x <> r

    concatWith f xs = foldr f mempty xs

    n = numerator u
    d = denominator u

latexFactor :: Expr -> [Char]
latexFactor (Sqrt x) = "\\sqrt{" <> latexBase x <> "}"
latexFactor (Pow x y) = latexBase x <> "^{" <> latexBase y <> "}" -- El exponente se imprime usando 'latexBase' para desambiguar expresiones como x**y**z
latexFactor (Exp 1) = "e"
latexFactor (Exp x) = "e" <> "^{" <> latexBase x <> "}"
latexFactor u = latexBase u

latexBase :: Expr -> [Char]
latexBase (Number n)
  | printAsFraction n = "(" <> show n <> ")"
  | otherwise = show n
latexBase Pi = "\\pi"
latexBase (Symbol s) = s
latexBase u@(Exp _) = "(" <> latexFactor u <> ")"
-- Derivadas
latexBase (Derivative u x) = "\\dfrac{d}{d" <> latexBase x <> "}(" <> latexExpression u <> ")"
-- Integrales
latexBase (Integral u x) = "\\int (" <> latexExpression u <> ") d" <> latexBase x
latexBase (DefiniteIntegral u x a b) =
  "\\int_{" <> latexBase a <> "}^{" <> latexBase b <> "}(" <> latexExpression u <> ") d" <> latexBase x
latexBase (Fun name us) = "\\operatorname{" <> name <> "}(" <> concatWith (surround ",") (fmap latexExpression us) <> ")"
  where
    surround x l r = l <> x <> r
    concatWith f xs = foldr f mempty xs
latexBase u = "(" <> latexExpression u <> ")"

latex :: Expr -> [Char]
latex = latexExpression