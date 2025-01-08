module Simplification.TrigExpand where
    
import qualified Simplification.Algebraic as Algebraic

import Expr
import PExpr

expr :: PExpr -> Expr
expr = return

expandTrig' :: PExpr -> Expr
expandTrig' (Sin (Add [])) = 0
expandTrig' (Cos (Add [])) = 1

expandTrig' (Sin (Add [x])) = sin $ expr x
expandTrig' (Cos (Add [x])) = cos $ expr x

expandTrig' (Sin (Add (x:xs))) = do
                                    sx <- sin $ return x
                                    cx <- cos $ return x
                                    sxs <- expandTrig' $ Sin $ Add xs
                                    cxs <- expandTrig' $ Cos $ Add xs
                                    return $ sx * cxs + cx * sxs
expandTrig' (Cos (Add (x:xs))) = do
                                    sx <- sin $ expr x
                                    cx <- cos $ expr x
                                    sxs <- expandTrig' $ Sin $ Add xs
                                    cxs <- expandTrig' $ Cos $ Add xs
                                    return $ cx * cxs - sx * sxs         


-- falta expandtrig sin(n*x) cuando n es entero y derivados

expandTrig' x = return x

expandTrig :: Expr -> Expr
expandTrig x = Algebraic.expand $ x >>= expandTrig'
