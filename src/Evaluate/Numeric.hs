{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Evaluate.Numeric (
    eval
) where
import PExpr
import Classes.EvalSteps (EvalSteps)
import Symplify (simplifySum, simplifyProduct, simplifyPow, simplifyFun)

import Control.Monad

import qualified Number as N

type Expr = EvalSteps PExpr
type Context = [(String, PExpr)]

evalPow :: PExpr -> PExpr -> Expr
evalPow (Number n) (Number m) = return $ Number (n ** m)
evalPow x y = simplifyPow x y

unOpNumber :: (t -> N.Number) -> t -> Expr
unOpNumber f n = return $ Number $ f n

evalFun :: PExpr -> Expr
evalFun (Sin (Number n)) = unOpNumber sin n
evalFun (Cos (Number n)) = unOpNumber cos n
evalFun (Tan (Number n)) = unOpNumber tan n
evalFun (Asin (Number n)) = unOpNumber asin n
evalFun (Acos (Number n)) = unOpNumber acos n
evalFun (Atan (Number n)) = unOpNumber atan n
evalFun (Sinh (Number n)) = unOpNumber sinh n
evalFun (Cosh (Number n)) = unOpNumber cosh n
evalFun (Tanh (Number n)) = unOpNumber tanh n
evalFun (Asinh (Number n)) = unOpNumber asinh n
evalFun (Acosh (Number n)) = unOpNumber acosh n
evalFun (Atanh (Number n)) = unOpNumber atanh n
evalFun (Exp (Number n)) = unOpNumber exp n
evalFun (Log (Number n)) = unOpNumber log n
evalFun f@(Fun _ _) = simplifyFun f
evalFun x = return x

eval' :: PExpr -> Context -> Expr
eval' u@(Number _) _ = return u
eval' (Symbol s) c = case lookup s c of
                      Just x -> return x
                      Nothing -> return $ Symbol s
eval' (Add us) c = forM us (`eval'` c) >>= simplifySum
eval' (Mul us) c = forM us (`eval'` c) >>= simplifyProduct
eval' (Pow x y) c = do
                    x' <- eval' x c
                    y' <- eval' y c
                    evalPow x' y'
eval' (Fun f us) c = forM us (`eval'` c) >>= evalFun . Fun f

eval :: EvalSteps PExpr -> [(Expr, Expr)] -> EvalSteps PExpr
eval u c = do
            u' <- u
            c' <- mkContext c
            eval' u' c'
    where
        mkContext [] = return [("Pi", Number pi)]
        -- mkContext ((Symbol s, u):cs) = (s:) <$> mkContext cs
        mkContext (c:cs) = do
                            cs' <- mkContext cs
                            u <- fst c
                            case u of
                                Pi -> return cs' -- Ignore pi
                                Symbol s -> do
                                              v <- snd c
                                              return $ (s, v):cs'
                                _ -> return cs'