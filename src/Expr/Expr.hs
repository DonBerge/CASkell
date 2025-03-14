module Expr (
    -- | Definición del tipo 'Expr'
    module Expr.ExprType,
    -- | PatternSynonyms y funciones para manipular la estructura de expresiones
    module Expr.Structure,
    -- | Herramientas para manipular polinomios, como la división polinomica y el maximo común divisor
    module Expr.PolyTools,
    -- | Parser de expresiones
    module Expr.Parser,
    -- | PrettyPrint de expresiones
    module Expr.PrettyPrint,
    -- | Suposiciones
    module Assumptions
) where

import Expr.ExprType

import Expr.Structure

import Expr.PolyTools

import Expr.Parser

import Expr.PrettyPrint

import Assumptions