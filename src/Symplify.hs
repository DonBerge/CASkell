{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Symplify (
    module Classes.EvalSteps,
    automaticSymplify,
    simplifyProduct,
    simplifySum,
    simplifyDiv,
    simplifySub,
    simplifyPow,
    simplifyNegate,
    operands,
    mulByNeg,
    freeOf,
    linearForm,
    simplifyFun,
    module PExpr,
) where

import Prelude hiding (const, exponent)

import qualified Number as N
import PExpr

import Data.List
import Control.Applicative
import Classes.EvalSteps
import Control.Monad.Except (MonadError(throwError))


numberNumerator :: PExpr -> Integer
numberNumerator (Number x) = N.numerator x
numberNumerator _ = error "numberNumerator: not a number"

-- instance Real PExpr where
--     toRational (Number x) = x
--     toRational _ = error "toRational: not a number"

automaticSymplify :: PExpr -> EvalSteps PExpr
automaticSymplify (Mul xs) = mapM automaticSymplify xs >>= simplifyProduct
automaticSymplify (Add xs) = mapM automaticSymplify xs >>= simplifySum
automaticSymplify (Pow x y) = do
                                x' <- automaticSymplify x
                                y' <- automaticSymplify y
                                simplifyPow x' y'
automaticSymplify (Fun f xs) = do
                                    xs' <- mapM automaticSymplify xs
                                    simplifyFun $ Fun f xs'
automaticSymplify x = return x

------------------------------
isConstant :: PExpr -> Bool
isConstant (Number _) = True
-- isConstant Pi = True
isConstant _ = False

const :: PExpr -> PExpr
const (Mul []) = 1
const (Mul (x:_))
    | isConstant x = x
    | otherwise = 1
const u
    | isConstant u = error "Constants dont have terms"
    | otherwise = 1

term :: PExpr -> PExpr
term (Mul []) = error "Empty products do not have terms"
term (Mul (x:xs))
    | isConstant x = Mul xs
    | otherwise = Mul (x:xs)
term u
    | isConstant u = error "Constants dont have terms"
    | otherwise = Mul [u]

base :: PExpr -> PExpr
base (Number _) = error "Base of a number is not defined"
-- base (Exp _) = Exp 1
base (Pow x _) = x
base u = u

exponent :: PExpr -> PExpr
exponent (Number _) = error "Exponent of a number is not defined"
-- exponent (Exp x) = x
exponent (Pow _ x) = x
exponent _ = 1

---------------------------------------------------------------------------------------

-- SPOW-2
simplifyPow :: PExpr -> PExpr -> EvalSteps PExpr
simplifyPow 0 w
    | true $ isPositive w = return 0
    | otherwise = throwError "Division por cero"
simplifyPow 1 _ = return 1
simplifyPow v w
    | true $ isInteger w = simplifyIntPow v (numberNumerator w)
    | otherwise = return (Pow v w)
    where
        simplifyIntPow (Number x) n = return $ Number $ x**fromIntegral n
        simplifyIntPow _ 0 = return 1
        simplifyIntPow x 1 = return x
        simplifyIntPow (Pow r s) n = simplifyProduct [s,fromInteger n] >>= simplifyPow r
        simplifyIntPow (Mul r) n = mapM (`simplifyIntPow` n) r >>= simplifyProduct
        simplifyIntPow x n = return $ Pow x (fromInteger n)

simplifyProduct :: [PExpr] -> EvalSteps PExpr
simplifyProduct [] = return 1
simplifyProduct [x] = return x -- SPRD.3
simplifyProduct xs
    | 0 `elem` xs = return 0 -- SPRD.2
    | otherwise = do
                    xs' <- simplifyProductRec xs
                    case xs' of -- SPRD.4
                        [] -> return 1
                        [x] -> return x
                        [u, Add vs] | isConstant u -> Add . sort <$> mapM (simplifyProduct . reverse . (:[u])) vs
                        _ -> return $ Mul $ sort xs'

--simplifyProductRec = undefined
-- SPRDREC-2
simplifyProductRec :: [PExpr] -> EvalSteps [PExpr]
simplifyProductRec [] = return []
simplifyProductRec [Mul us, Mul vs] = mergeProducts us vs
simplifyProductRec [Mul us, v] = mergeProducts us [v]
simplifyProductRec [u, Mul vs] = mergeProducts [u] vs
---- SPRDREC-1
simplifyProductRec [Number u, Number v]
    | u * v == 1 = return []
    | otherwise = return [Number $ u * v]

simplifyProductRec [Number 1, v] = return [v]
simplifyProductRec [u, Number 1] = return [u]
simplifyProductRec [u,v]
    | v < u = simplifyProductRec [v,u]
    | isConstant u = return [u,v] -- evita un error en el caso de abajo
    | base u == base v = do
                            s <- simplifySum [exponent u, exponent v]
                            p <- simplifyPow (base u) s
                            if p == 1
                                then return []
                                else return [p]
    | otherwise = return [u,v]
-- SPRDREC-3
simplifyProductRec ((Mul us):vs) = simplifyProductRec vs >>= mergeProducts us
simplifyProductRec (u:vs) = simplifyProductRec vs >>= mergeProducts [u]

simplifyDiv :: PExpr -> PExpr -> EvalSteps PExpr
simplifyDiv x y = do
                    y' <- simplifyPow y (-1)
                    simplifyProduct [x, y']

simplifySum :: [PExpr] -> EvalSteps PExpr
simplifySum [] = return 0
simplifySum [x] = return x
simplifySum xs = do
                    xs' <- simplifySumRec xs
                    case xs' of -- SPRD.4
                        [] -> return 0
                        [x] -> return x
                        _ -> return $ Add $ sort xs'

simplifySumRec :: [PExpr] -> EvalSteps [PExpr]
simplifySumRec [] = return []
simplifySumRec [Add us, Add vs] = mergeSums us vs
simplifySumRec [Add us, v] = mergeSums us [v]
simplifySumRec [u, Add vs] = mergeSums [u] vs
---- SPRDREC-1
simplifySumRec [Number u, Number v]
    | u + v == 0 = return []
    | otherwise = return [Number $ u + v]

simplifySumRec [0, v] = return [v]
simplifySumRec [u, 0] = return [u]
simplifySumRec [u, v]
    | v < u = simplifySumRec [v,u]
    | isConstant u  = return [u,v] -- evita undefined en el caso de abajo
    | term u == term v = let
                            vt = term v
                            uc = const u
                            vc = const v
                         in do
                                s <- simplifySum [uc, vc]
                                p <- simplifyProduct [s, vt]
                                if p == 0
                                    then return []
                                    else return [p]
    | otherwise = return [u,v]
-- SPRDREC-3
simplifySumRec ((Add us):vs) = simplifySumRec vs >>= mergeSums us
simplifySumRec (u:vs) = simplifySumRec vs >>= mergeSums [u]

simplifySub :: PExpr -> PExpr -> EvalSteps PExpr
simplifySub x y = do
                    y' <- simplifyProduct [y, -1]
                    simplifySum [x, y']

mergeOps :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> [a] -> m [a]
mergeOps _ p [] = return p
mergeOps _ [] q = return q
mergeOps f (p:ps) (q:qs) = do
                            h <- f [p, q]
                            case h of
                                [] -> mergeOps f ps qs
                                [h'] -> (h':) <$> mergeOps f ps qs
                                [r,_] -> if p == r
                                                then (p:) <$> mergeOps f ps (q:qs)
                                                else (q:) <$> mergeOps f (p:ps) qs
                                _ -> error "mergeOps: unexpected pattern"

mergeProducts :: [PExpr] -> [PExpr] -> EvalSteps [PExpr]
mergeProducts = mergeOps simplifyProductRec

mergeSums :: [PExpr] -> [PExpr] -> EvalSteps [PExpr]
mergeSums = mergeOps simplifySumRec

simplifyNegate :: PExpr -> EvalSteps PExpr
simplifyNegate a = simplifyProduct [a, -1]

operands :: PExpr -> [PExpr]
operands (Add xs) = xs
operands (Mul xs) = xs
operands (Pow x y) = [x, y]
operands  (Fun _ xs) = xs
operands _ = []


freeOf :: PExpr -> PExpr -> Bool
freeOf u t
    | u == t = False
freeOf (Symbol _) _ = True
freeOf (Number _) _ = True
freeOf u t = all (`freeOf` t) $ operands u


linearForm :: PExpr -> PExpr -> EvalSteps (PExpr, PExpr)
linearForm u x
    | u == x = return (1, 0)
    | notASymbol x = throwError $ show x ++ " must be a symbol"
        where
            notASymbol (Symbol _) = False
            notASymbol _ = True
linearForm u@(Number _) _ = return (0, u)
linearForm u@(Symbol _) _ = return (0, u)
linearForm u@(Mul _) x
    | freeOf u x = return (0, u)
    | otherwise = do
                    udivx <- simplifyDiv u x
                    if freeOf udivx x
                        then return (udivx, 0)
                        else throwError "not a linear form"
linearForm u@(Add []) _ = return (0, u)
linearForm (Add (u:us)) x = do
                                (a,b) <- linearForm u x
                                (c,d) <- linearForm (Add us) x
                                a' <- simplifySum [a, c]
                                b' <- simplifySum [b, d]
                                return (a', b')
linearForm u x
    | freeOf u x = return (0, u)
    | otherwise = throwError "not a linear form"

mulByNeg :: PExpr -> Bool
mulByNeg (Mul ((Number a):_)) = a<0
mulByNeg (Add xs) = all mulByNeg xs
mulByNeg x = true $ isNegative x

simplifySqrt :: PExpr -> EvalSteps PExpr
simplifySqrt x = simplifyPow x 0.5

----------------

handlePeriod :: (N.Number -> PExpr -> EvalSteps PExpr) -> (PExpr -> EvalSteps PExpr) -> PExpr -> EvalSteps PExpr
handlePeriod cases onOddPi x = do
                    p <- linearForm x Pi
                    case p of
                        (Number n, b) -> let
                                            (m, r) = properFraction n
                                            q = cases r b
                                         in if even m
                                                then q
                                                else q >>= onOddPi
                        _ -> throwError "Could not handle period"

simplifyFun :: PExpr -> EvalSteps PExpr
simplifyFun (Sin x)
    | mulByNeg x = simplifyNegate x >>= simplifyFun . Sin >>= simplifyNegate
simplifyFun (Sin x) = handlePeriod cases simplifyNegate x <|> return (Sin x)
    where
        cases r b = case (r,b) of
                        (0,0) -> return 0
                        (0,_) -> if mulByNeg b
                                    then simplifyNegate b >>= simplifyNegate . Sin
                                    else return $ Sin b
                        (_,0) | r==1/6 -> return $ 1/2
                        (_,0) | r==1/4 -> simplifySqrt 2 >>= (`simplifyDiv` 2)
                        (_,0) | r==1/3 -> simplifySqrt 3 >>= (`simplifyDiv` 2)
                        (_,0) | r==1/2 -> return 1
                        (_,_) -> Sin <$> (simplifyProduct [Number r,Pi] >>= simplifySum . (:[b]))
simplifyFun (Cos x)
    | mulByNeg x = simplifyNegate x >>= simplifyFun . Cos
simplifyFun (Cos x) = handlePeriod cases simplifyNegate x <|> return (Cos x)
    where
        cases r b = case (r,b) of
                        (0,0) -> return 1
                        (0,_) -> if mulByNeg b
                                    then Cos <$> simplifyNegate b
                                    else return $ Cos b
                        (_,0) | r==1/6 -> simplifySqrt 3 >>= (`simplifyDiv` 2)
                        (_,0) | r==1/4 -> simplifySqrt 2 >>= (`simplifyDiv` 2)
                        (_,0) | r==1/3 -> return $ 1/2
                        (_,0) | r==1/2 -> return 0
                        (_,_) -> Cos <$> (simplifyProduct [Number r,Pi] >>= simplifySum . (:[b]))
simplifyFun (Tan x)
    | mulByNeg x = simplifyNegate x >>= simplifyFun . Tan >>= simplifyNegate
simplifyFun (Tan x) = handlePeriod cases return x <|> return (Tan x)
    where
        cases r b = case (r,b) of
                        (0,0) -> return 0
                        (0,_) -> if mulByNeg b
                                    then simplifyNegate b >>= simplifyNegate . Tan
                                    else return $ Tan b
                        (_,0) | r==1/6 -> return $ 1/3
                        (_,0) | r==1/4 -> return 1
                        (_,0) | r==1/3 -> simplifySqrt 3
                        (_,0) | r==1/2 -> return $ 1/0
                        (_,_) -> Tan <$> (simplifyProduct [Number r,Pi] >>= simplifySum . (:[b]))

-- exponenciales
simplifyFun (Exp x) = do 
                        y' <- simplifyPow e x
                        case y' of
                            Pow (Exp 1) y -> return $ Exp y
                            y -> return y
    where
        e = Exp 1


simplifyFun x = return x


------------------
----