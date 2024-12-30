{-# LANGUAGE PatternSynonyms #-}
module Trigsimp where

import PExpr

import Symplify (simplifyProduct, simplifySum)
import Expr
import Control.Monad

import Data.List

pattern Sum :: PExpr -> PExpr -> PExpr
pattern Sum x y = Add [x,y]

pattern Prod :: PExpr -> PExpr -> PExpr
pattern Prod x y = Mul [x,y]

pattern Neg :: [PExpr] -> PExpr
pattern Neg xs = Mul ((-1):xs)

-- Funciones basadas em "AUTOMATED AND READABLE SIMPLIFICATION OF TRIGONOMETRIC EXPRESSIONS" de Hongguang Fu
expr :: PExpr -> Expr
expr = return

tr1 :: PExpr -> Expr
tr1 (Sec x) = 1 / (cos (expr x))
tr1 (Csc x) = 1 / (sin (expr x))
tr1 x = expr x

tr2 :: PExpr -> Expr
tr2 (Tan x) = let x' = expr x in sin x' / cos x'
tr2 (Cot x) = let x' = expr x in cos x' / sin x'
tr2 x = expr x

tr3 :: PExpr -> Expr
-- Row 1
tr3 (Sin (Neg x)) = negate $ sin $ expr $ Mul x
tr3 (Cos (Neg x)) = cos $ expr $ Mul x
tr3 (Tan (Neg x)) = negate $ tan $ expr $ Mul x
tr3 (Cot (Neg x)) = negate $ cot $ expr $ Mul x
-- Row 2
--- TODO
--tr3 (Sin (Sum Pi y)) =
tr3 x = expr x

tr4 :: PExpr -> Expr
tr4 (Sin (Prod (Number a) Pi))
    | a == 0 = 0
    | a == 1/6 = 1/2
    | a == 1/4 = sqrt 2 / 2
    | a == 1/3 = sqrt 3 / 2
    | a == 1/2 = 1
tr4 (Cos (Prod (Number a) Pi))
    | a == 0 = 1
    | a == 1/6 = sqrt 3 / 2
    | a == 1/4 = sqrt 2 / 2
    | a == 1/3 = 1/2
    | a == 1/2 = 0
tr4 (Tan (Prod (Number a) Pi))
    | a == 0 = 0
    | a == 1/6 = sqrt 3 / 3
    | a == 1/4 = 1
    | a == 1/3 = sqrt 3
    | a == 1/2 = fail "tan(π/2) is undefined"
tr4 x = expr x

tr5 :: PExpr -> Expr
tr5 (Pow (Sin x) (Number 2)) = 1 - cos ((expr x) ** 2)
tr5 x = expr x

tr6 :: PExpr -> Expr
tr6 (Pow (Cos x) (Number 2)) = 1 - sin ((expr x) ** 2)
tr6 x = expr x

tr7 :: PExpr -> Expr
tr7 (Pow (Cos x) (Number 2)) = (1 + cos (2 * expr x)) / 2
tr7 x = expr x


tr8 :: PExpr -> Expr
tr8 (Mul l) = tr8' l >>= simplifyProduct
    where
        tr8' ((Sin a):(Cos b):xs) = let 
                                        a' = expr a 
                                        b' = expr b 
                                    in 
                                        liftA2 (:) ((1/2) * (sin (a' + b') + sin (a' - b'))) (tr8' xs)
        tr8' ((Cos b):(Sin a):xs) = let 
                                        a' = expr a 
                                        b' = expr b 
                                    in 
                                        liftA2 (:) ((1/2) * (sin (a' + b') + sin (a' - b'))) (tr8' xs)
        tr8' ((Sin a):(Sin b):xs) = let 
                                        a' = expr a 
                                        b' = expr b 
                                    in 
                                        liftA2 (:) (-(1/2) * (cos (a' + b') - cos (a' - b'))) (tr8' xs)
        tr8' ((Cos a):(Cos b):xs) = let 
                                        a' = expr a 
                                        b' = expr b 
                                    in 
                                        liftA2 (:) ((1/2) * (cos (a' + b') + cos (a' - b'))) (tr8' xs)
        tr8' (x:y:xs) = liftA2 (:) (expr x) (tr8' (y:xs))
        tr8' xs = return xs

tr8 x = expr x

tr9 :: PExpr -> Expr
tr9 (Add l) = tr9' l >>= simplifySum
    where
        tr9' ((Sin a):(Sin b):xs) = liftA2 (:) (2 * sin ((expr a + expr b) / 2) * cos ((expr a - expr b) / 2)) (tr9' xs)
        tr9' ((Cos a):(Cos b):xs) = liftA2 (:) (2 * cos ((expr a + expr b) / 2) * cos ((expr a - expr b) / 2)) (tr9' xs)
        tr9' ((Sin a):(Cos b):xs) = liftA2 (:) (2 * sin ((expr a + expr b) / 2) * sin ((expr a - expr b) / 2)) (tr9' xs)
        tr9' ((Cos a):(Sin b):xs) = liftA2 (:) (2 * sin ((expr a + expr b) / 2) * cos ((expr a - expr b) / 2)) (tr9' xs)
        tr9' (x:y:xs) = liftA2 (:) (expr x) (tr9' (y:xs))
        tr9' xs = return xs
tr9 x = expr x

tr10 :: PExpr -> Expr
tr10 (Sin (Sum a b)) = sin (expr a) * cos (expr b) + cos (expr a) * sin (expr b)
tr10 (Cos (Sum a b)) = cos (expr a) * cos (expr b) - sin (expr a) * sin (expr b)
tr10 x = expr x

tr10i :: PExpr -> Expr
tr10i (Sum (Prod (Sin a) (Cos b)) (Prod (Cos c) (Sin d)) ) 
    | a == c && b == d = sin (expr a + expr b)
--     | otherwise = expr e
tr10i (Sum (Prod (Cos a) (Cos b)) (Prod (Sin c) (Sin d)) ) 
    | a == c && b == d = cos (expr a + expr b)
tr10i e = expr e
    -- = sin (expr a) * cos (expr b) + cos (expr a) * sin (expr b)

tr11 :: PExpr -> Expr
tr11 (Sin (Prod 2 x)) = 2 * sin (expr x) * cos (expr x)
tr11 (Cos (Prod 2 x)) = 2 * cos (expr x) ** 2 - 1
tr11 x = expr x

tr12 :: PExpr -> Expr
tr12 (Tan (Sum a b)) = (tan (expr a) + tan (expr b)) / (1 - tan (expr a) * tan (expr b))
tr12 x = expr x

tr13 :: PExpr -> Expr
tr13 (Mul l) = tr13' l >>= simplifyProduct
    where
        tr13' ((Tan a):(Tan b):xs) = liftA2 (:) (1-(tan (expr a) + tan (expr b)) * cot ((expr a)+(expr b))) (tr13' xs)
        tr13' ((Cot a):(Cot b):xs) = liftA2 (:) (1+(cot (expr a) + cot (expr b)) * tan ((expr a)+(expr b))) (tr13' xs)
        tr13' xs = return xs
tr13 x = expr x

tr0 :: PExpr -> Expr
tr0 = return

lenTrig :: PExpr -> Int
lenTrig = undefined

minLExp :: [PExpr] -> PExpr
minLExp = minimumBy (\x y -> compare (lenTrig x) (lenTrig y))

ctr1 :: PExpr -> Expr
ctr1 f = do
            f1 <- tr5 f >>= tr0
            f2 <- tr6 f >>= tr0
            return $ minLExp [f1,f2,f]

ctr2 :: PExpr -> Expr
ctr2 f = do
            f3 <- tr11 f
            f1 <- tr5 f3
            f2 <- tr6 f3
            return $ minLExp [f1,f2,f3]

ctr3 :: PExpr -> Expr
ctr3 f = do
            f1 <- tr8 f
            f2 <- tr10i f1
            return $ minLExp [f1,f2,f]

ctr4 :: PExpr -> Expr
ctr4 f = do
            f2 <- tr4 f >>= tr10i
            return $ minLExp [f,f2]

exists :: [String] -> PExpr -> Bool
exists xs (Fun f ys) = f `elem` xs || any (exists xs) ys
exists xs (Add ys) = any (exists xs) ys
exists xs (Mul ys) = any (exists xs) ys
exists xs (Pow x y) = exists xs x || exists xs y
exists _ _ = False

composeM :: Monad m => [(a -> m a)] -> (a -> m a)
composeM = foldr (>=>) return

rl1 :: PExpr -> Expr
rl1 = composeM [tr4,tr3,tr4,tr12,tr4,tr13,tr4,tr0]

rl2 :: PExpr -> Expr
rl2 = composeM [tr4,tr3,tr10,tr4,tr3,tr11,tr5,tr7,tr11,tr4,ctr3,tr0,ctr1,tr9,ctr2,tr4,tr9,tr0,tr9,ctr4]


appRule :: (PExpr -> Expr) -> PExpr -> Expr
appRule f a = f a >>= appRule' (appRule f)
    where 
        appRule' g (Add xs) = Add <$> mapM (appRule g) xs
        appRule' g (Mul xs) = Mul <$> mapM (appRule g) xs
        appRule' g (Pow x y) = Pow <$> appRule g x <*> appRule g y
        appRule' g (Fun x xs) = Fun x <$> mapM (appRule g) xs
        appRule' g x = g x

trigSimp :: PExpr -> Expr
trigSimp e = do
                e1 <- appRule tr1 e
                e2 <- if exists ["Tan", "Cot"] e1
                        then do
                                e' <- appRule rl1 e1
                                return $ minLExp [e1, e']
                        else return e1
                e3 <- appRule tr3 e2 >>= appRule tr0
                if exists ["Sin", "Cos"] e3
                    then do
                            e' <- appRule rl2 e3
                            return $ minLExp [e1, e']
                    else return e3


