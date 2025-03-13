{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
  )
where

import Data.Foldable (toList)
import Data.Function
import Data.TwoList (reverse, sortBy)

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
      (v :|| vs) = reverse $ sortBy (compare `on` (multidegree vars)) us -- ordenar los monomios segun el multigrado
   in cat $ prettyTerm v : map addSigns (toList vs)
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