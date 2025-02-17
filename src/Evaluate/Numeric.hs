-- permite pasar funciones polimorficas como argumento
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Evaluate.Numeric
  ( eval,
  )
where

import Expr
import qualified Number as N
import Structure

type Context = [(Expr, N.Number)]

-- mkUnaryNumberOp :: (a -> N.Number) -> a -> Expr
-- mkUnaryNumberOp f = construct . Number . f

-- |
--    @evalFloatingOp f x@ sirve para evaluar @f x@, si @x@ es un numero entonces devuelve el valor numerico aproximado,
--    sino retorna la expresión simbolica.
evalFloatingOp :: (forall a. (Floating a) => a -> a) -> Expr -> Expr
evalFloatingOp f (structure -> Number n) = construct $ Number $ f n
evalFloatingOp f x = f x

removeElement :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeElement _ [] = []
removeElement x ((y, z) : xs)
  | x == y = xs
  | otherwise = (y, z) : removeElement x xs

eval :: Context -> Expr -> Expr
eval ctx u@(structure -> Symbol _) = maybe u (construct . Number) $ lookup u ctx
eval ctx (structure -> Derivative u x) = construct $ Derivative (eval (removeElement x ctx) u) x -- no evaluar la variable de derivacion
eval ctx (structure -> Integral u x) = construct $ Integral (eval (removeElement x ctx) u) x -- no evaluar la variable de integracion
eval ctx (structure -> DefiniteIntegral u x a b) =
  let u' = eval (removeElement x ctx) u
      a' = eval ctx a
      b' = eval ctx b
   in construct $ DefiniteIntegral u' x a' b' -- no evaluar la variable de integracion
eval ctx u = eval' $ mapStructure (eval ctx) u
  where
    -- evaluar funciones con numeros como argumento, ademas de potencias de numeros
    eval' (structure -> Pow a b)
      | Number n <- structure a, Number m <- structure b = construct $ Number $ n ** m -- potencias de numeros
    eval' (structure -> Sin x) = evalFloatingOp sin x -- uno de los sin evalua a numeros y otro a expresiones
    eval' (structure -> Cos x) = evalFloatingOp cos x
    eval' (structure -> Tan x) = evalFloatingOp tan x
    eval' (structure -> Asin x) = evalFloatingOp asin x
    eval' (structure -> Acos x) = evalFloatingOp acos x
    eval' (structure -> Atan x) = evalFloatingOp atan x
    eval' (structure -> Sinh x) = evalFloatingOp sinh x
    eval' (structure -> Cosh x) = evalFloatingOp cosh x
    eval' (structure -> Tanh x) = evalFloatingOp tanh x
    eval' (structure -> Asinh x) = evalFloatingOp asinh x
    eval' (structure -> Acosh x) = evalFloatingOp acosh x
    eval' (structure -> Atanh x) = evalFloatingOp atanh x
    eval' (structure -> Exp x) = evalFloatingOp exp x
    eval' (structure -> Log x) = evalFloatingOp log x
    eval' v = v

{-
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
-}