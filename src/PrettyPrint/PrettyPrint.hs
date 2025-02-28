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
module PrettyPrint (
  pretty
) where

import Data.Foldable (toList)
import Data.Function
import Data.TwoList (reverse, sortBy)
import Expr
import Prettyprinter
import Simplification.PolyTools
import Structure
import Prelude hiding (reverse)

-- * Pretty printing de los simbolos no terminales

prettyExpression :: Expr -> Doc ann
prettyExpression u@(Add us) =
  let vars = variables u
      (v :|| vs) = reverse $ sortBy (compare `on` (multidegree vars)) us -- ordenar los monomios segun el multigrado
   in fillSep $ prettyTerm v : map addSigns (toList vs)
  where
    -- Agrega un operador + o - dependiendo del elemento
    addSigns (Neg y) = pretty "-" <+> prettyTerm y
    addSigns y = pretty "+" <+> prettyTerm y
prettyExpression u = prettyTerm u

prettyTerm :: Expr -> Doc ann
prettyTerm (Mul us) = concatWith (surround (pretty "*")) $ fmap prettyFactor us
prettyTerm u = prettyFactor u

prettyFactor :: Expr -> Doc ann
prettyFactor (Pow x@(Exp _) y) = parens (prettyBase x) <> pretty "^" <> prettyBase y -- Caso especial, la exponencial se repreenta como e^x
prettyFactor (Pow x y) = prettyBase x <> pretty "^" <> prettyBase y -- El exponente se imprime usando 'prettyBase' para desambiguar expresiones como x**y**z
prettyFactor u = prettyBase u

prettyBase :: Expr -> Doc ann
prettyBase (Number n) = viaShow n
prettyBase (Symbol s) = pretty s
prettyBase (Exp x) = pretty "e" <> pretty "^" <> prettyBase x
prettyBase (Fun name us) = pretty name <> parens (concatWith (surround comma) (fmap prettyExpression us))
prettyBase u = parens $ prettyExpression u

-- * Funcion de pretty printing

instance Pretty Expr where
  pretty u =
    let n = numerator u
        d = denominator u
     in if d == 1
          then prettyExpression n
          else prettyTerm n <> slash <> prettyTerm d
