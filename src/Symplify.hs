{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Symplify where

import Prelude hiding (const, exponent)

import qualified Number as N
import PExpr

import Data.List

numberNumerator :: PExpr -> Integer
numberNumerator (Number x) = N.numerator x
numberNumerator _ = error "numberNumerator: not a number"

numberDenominator :: PExpr -> Integer
numberDenominator (Number x) = N.denominator x
numberDenominator _ = error "numberDenominator: not a number"

getNumber :: PExpr -> N.Number
getNumber (Number x) = x
getNumber _ = error "getNumber: not a number"

-- instance Real PExpr where
--     toRational (Number x) = x
--     toRational _ = error "toRational: not a number"

automaticSymplify :: MonadFail m => PExpr -> m (PExpr)
automaticSymplify (Mul xs) = mapM automaticSymplify xs >>= simplifyProduct
automaticSymplify (Add xs) = mapM automaticSymplify xs >>= simplifySum
automaticSymplify (Pow x y) = do
                                x' <- automaticSymplify x
                                y' <- automaticSymplify y
                                simplifyPow x' y'
automaticSymplify (Fun f xs) = do
                                    xs' <- mapM automaticSymplify xs
                                    simplifyFunction $ Fun f xs'
automaticSymplify x = return x

isPositive :: PExpr -> Bool
isPositive (Number x) = x > 0
isPositive (Exp _) = True
isPositive _ = False

isNegative :: PExpr -> Bool
isNegative (Number x) = x < 0
isNegative (Exp _) = False
isNegative _ = False

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


isInteger :: PExpr -> Bool
isInteger (Number x) = N.isInteger x
isInteger _ = False

fromNumber :: PExpr -> Double
fromNumber (Number x) = N.fromNumber x
fromNumber _ = error "fromNumber: not a number"

simplifyPow :: MonadFail m => PExpr -> PExpr -> m (PExpr)
-- SPOW-2
simplifyPow 0 w
    | isPositive w = return 0
    | otherwise = fail "0^w is not defined for w <= 0"
simplifyPow 1 _ = return 1
simplifyPow v w
    | isInteger w = simplifyIntPow v (numberNumerator w)
    | otherwise = return (Pow v w)
    where
        simplifyIntPow (Number x) n = return $ Number $ x**fromIntegral n
        simplifyIntPow _ 0 = return 1
        simplifyIntPow x 1 = return x
        simplifyIntPow (Pow r s) n = simplifyProduct [s,fromInteger n] >>= simplifyPow r
        simplifyIntPow (Mul r) n = mapM (`simplifyIntPow` n) r >>= simplifyProduct
        simplifyIntPow x n = return $ Pow x (fromInteger n)

simplifyProduct :: MonadFail m => [PExpr] -> m (PExpr)
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

simplifyProductRec :: MonadFail m => [PExpr] -> m [PExpr]
--simplifyProductRec = undefined
-- SPRDREC-2
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

simplifySum :: MonadFail m => [PExpr] -> m (PExpr)
simplifySum [] = return 0
simplifySum [x] = return x
simplifySum xs = do
                    xs' <- simplifySumRec xs
                    case xs' of -- SPRD.4
                        [] -> return 0
                        [x] -> return x
                        _ -> return $ Add $ sort xs'

simplifySumRec :: MonadFail m => [PExpr] -> m [PExpr]
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

mergeProducts :: MonadFail m => [PExpr] -> [PExpr] -> m [PExpr]
mergeProducts = mergeOps simplifyProductRec

mergeSums :: MonadFail m => [PExpr] -> [PExpr] -> m [PExpr]
mergeSums = mergeOps simplifySumRec

simplifyFunction :: Monad m => a -> m a
simplifyFunction = return