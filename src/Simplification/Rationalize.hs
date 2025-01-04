module Simplification.Rationalize where

import PExpr
import Symplify
import Expr
import Symplify (simplifyProduct)

rationalize :: PExpr -> Expr
rationalize (Pow x y) = rationalize x >>= (`simplifyPow` y)
rationalize (Mul xs) = mapM rationalize xs >>= simplifyProduct

