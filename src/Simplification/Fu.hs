{-# LANGUAGE PatternSynonyms #-}
module Simplification.Fu where

import PExpr

import Symplify

import Expr

import Control.Monad

import Data.Function
import Data.List ( minimumBy )
import Simplification.PolyTools (rationalSimplify)

import qualified Simplification.Algebraic as Algebraic

pattern Neg :: PExpr -> PExpr
pattern Neg x <- Mul ((-1):x:_)

bottomUp :: (PExpr -> Expr) -> PExpr -> Expr
bottomUp f (Add xs) = mapM (bottomUp f) xs >>= simplifySum >>= f
bottomUp f (Mul xs) = mapM (bottomUp f) xs >>= simplifyProduct >>= Algebraic.expand' >>= f
bottomUp f (Pow x y) = do
                        x' <- bottomUp f x
                        y' <- bottomUp f y
                        simplifyPow x' y' >>= f
bottomUp f (Fun g xs) = mapM (bottomUp f) xs >>= simplifyFun . Fun g >>= f
bottomUp _ x = return x

tr0 :: PExpr -> EvalSteps PExpr
tr0 = rationalSimplify . return

tr1 :: PExpr -> Expr
tr1 = bottomUp tr1'
    where
        tr1' (Sec x) = simplifyDiv 1 (Cos x)
        tr1' (Csc x) = simplifyDiv 1 (Sin x)
        tr1' x = return x

tr2 :: PExpr -> Expr
tr2 = bottomUp tr2'
    where
        tr2' (Tan x) = simplifyDiv (Sin x) (Cos x)--let x' = return x in sin (x') / cos (x')
        tr2' (Cot x) = simplifyDiv (Cos x) (Sin x)--let x' = return x in cos (x') / sin (x')
        tr2' x = return x

tr2i :: PExpr -> Expr
tr2i = bottomUp tr2i'
    where
        tr2i' x = do
                    n <- numerator x
                    d <- denominator x
                    case (n,d) of
                        (Sin a, Cos b) | a==b -> return $ Tan a
                        (Cos a, Sin b) | a==b -> return $ Cot a
                        _ -> return x

tr3 :: PExpr -> Expr
tr3 = return

tr3' :: PExpr -> Expr
tr3' = return -- ya es manejado por autosimplificacion

tr4 :: PExpr -> Expr
tr4 = return -- ya es manejado por autosimplificacion

-- | Reemplaza sin^2(x) con 1-cos^2(x)
tr5 :: PExpr -> EvalSteps PExpr
tr5 = bottomUp tr5'
    where
        tr5' (Pow (Sin x) 2) = 1 - (Cos x `simplifyPow` 2)
        tr5' (Pow (Sin x) n)
            | true (isEven n &&& isPositive n) = let
                                    n' = n `simplifyDiv` 2
                                in
                                    tr5 (Pow (Sin x) 2) ** n'
        tr5' x = return x

tr6 :: PExpr -> Expr
tr6 = bottomUp tr6'
    where
        tr6' (Pow (Cos x) 2) = 1 - (Sin x `simplifyPow` 2)
        tr6' (Pow (Cos x) n)
            | true (isEven n &&& isPositive n) = let
                                    n' = n `simplifyDiv` 2
                                in
                                    tr6 (Pow (Cos x) 2) ** n'
        tr6' x = return x

tr7 :: PExpr -> Expr
tr7 = bottomUp tr7'
    where
        tr7' (Pow (Cos x) 2) = (cos (simplifyProduct [2,x]) + 1) / 2 --(1+cos(2*x'))/2
        tr7' x = return x

tr8 :: PExpr -> Expr
tr8 = bottomUp tr8'
    where
        --tr8Helper :: PExpr -> PExpr -> PExpr
        tr8Helper f g x y = let
                                a = simplifySum [x,y]
                                b = simplifySub x y
                            in
                                (1/2) * (g a `f` g b)

        tr8' (Mul []) = 1
        tr8' (Mul [x]) = return x
        tr8' (Mul ((Sin x):(Cos y):xs)) = tr8Helper (+) sin x y  * tr8' (Mul xs)
        tr8' (Mul ((Cos x):(Sin y):xs)) = tr8Helper (-) sin x y  * tr8' (Mul xs)

        tr8' (Mul ((Cos x):(Cos y):xs)) = tr8Helper (+) cos x y  * tr8' (Mul xs)
        tr8' (Mul ((Sin x):(Sin y):xs)) = negate $ tr8Helper (-) cos x y  * tr8' (Mul xs)

        tr8' (Mul (x@(Sin _):y:xs)) = tr8' (Mul (x:xs)) >>= simplifyProduct . (:[y])
        tr8' (Mul (x@(Cos _):y:xs)) = tr8' (Mul (x:xs)) >>= simplifyProduct . (:[y])
        tr8' (Mul (x:xs)) = tr8' (Mul xs) >>= simplifyProduct . (:[x])
        tr8' x = return x

tr9 :: PExpr -> Expr
tr9 = bottomUp tr9'
    where
        tr9Helper f g x y = let
                                a = simplifySum [x,y] >>= (`simplifyDiv` 2)
                                b = simplifySub x y >>= (`simplifyDiv` 2)
                            in
                                2 * f a * g b

        tr9' (Add []) = 0
        tr9' (Add [x]) = return x
        tr9' (Add (Sin x : Sin y : xs)) = tr9Helper sin cos x y + tr9' (Add xs)
        tr9' (Add (Sin x : Neg (Sin y) : xs)) = tr9Helper cos sin x y + tr9' (Add xs)

        tr9' (Add (Cos x : Cos y : xs)) = tr9Helper cos cos x y + tr9' (Add xs)
        tr9' (Add (Cos x : Neg (Cos y) : xs)) = tr9' (Add xs) - tr9Helper sin sin x y

        -- Inverso del caso anterior
        tr9' (Add ( x@(Neg (Sin _)):y@(Sin _):xs )) = tr9' (Add (y:x:xs))
        tr9' (Add ( x@(Neg (Cos _)):y@(Cos _):xs )) = tr9' (Add (y:x:xs))

        -- Propagar la funcion hacia adelante
        tr9' (Add (x@(Sin _) : y : xs)) = tr9' (Add (x:xs)) >>= simplifySum . (:[y])
        tr9' (Add (x@(Cos _) : y : xs)) = tr9' (Add (x:xs)) >>= simplifySum . (:[y]) 
        tr9' (Add (x@(Neg (Sin _)) : y : xs)) = tr9' (Add (x:xs)) >>= simplifySum . (:[y])
        tr9' (Add (x@(Neg (Cos _)) : y : xs)) = tr9' (Add (x:xs)) >>= simplifySum . (:[y])

        tr9' x = return x

tr10 :: PExpr -> Expr
tr10 = bottomUp tr10'
    where
        tr10Helper f g h x xs = do
                                    a <- tr10' (Cos (Add xs)) >>= simplifyProduct . (:[f x]) >>= tr3'
                                    b <- tr10' (Sin (Add xs)) >>= simplifyProduct . (:[g x]) >>= tr3'
                                    a `h` b

        tr10' (Sin (Add [])) = 0
        tr10' (Cos (Add [])) = 1

        tr10' (Sin (Add [x])) = return $ Sin x
        tr10' (Cos (Add [x])) = return $ Cos x

        tr10' (Sin (Add (x:xs))) = tr10Helper Sin Cos (\a b -> simplifySum [a,b]) x xs
        tr10' (Cos (Add (x:xs))) = tr10Helper Cos Sin simplifySub x xs
        
        tr10' x = return x

-- | First expand all products
-- | TR8 converts the product of two trigonometric functions into a sum of trigonometric functions
-- | Autosimplification cancels equal terms
tr10i :: PExpr -> Expr
tr10i = Algebraic.expand' >=> tr8

tr11 :: PExpr -> Expr
tr11 = bottomUp tr11'
    where
        tr11' x@(Sin (Mul [2])) = return x
        tr11' x@(Cos (Mul [2])) = return x
        tr11' (Sin (Mul (2:x))) = let x' = simplifyProduct x in 2 * sin x' * cos x'
        tr11' (Cos (Mul (2:x))) = let x' = simplifyProduct x in cos x' ** 2 -  sin x' ** 2
        tr11' (Sin (Mul (n:x)))
            | true $ isEven n = do
                                    n' <- n `simplifyDiv` 2
                                    x' <- simplifyProduct (n':x)
                                    s <- tr11' (Sin x')
                                    c <- tr11' (Cos x')
                                    simplifyProduct [2,s,c]
        tr11' (Cos (Mul (n:x)))
            | true $ isEven n = do
                                    n' <- n `simplifyDiv` 2
                                    x' <- simplifyProduct (n':x)
                                    s <- tr11' (Sin x') ** 2
                                    c <- tr11' (Cos x') ** 2
                                    simplifySub c s
                                    
        tr11' x = return x

tr12 :: PExpr -> Expr
tr12 = bottomUp tr12'
    where
        tr12' (Tan (Add xs)) = tr12'' xs
        tr12' x = return x

        tr12'' [] = 0
        tr12'' [x] = simplifyFun $ Tan x
        tr12'' (x:xs) = let
                            a = tr12'' [x]
                            b = tr12'' xs
                        in
                            (a + b) / (1 - a * b)

tr13 :: PExpr -> Expr
tr13 = bottomUp tr13'
    where
        partition [] = ([],[],[])
        partition (x:xs) = let
                                (a,b,c) = partition xs
                            in case x of
                                Tan _ -> (x:a,b,c)
                                Cot _ -> (a,x:b,c)
                                _ -> (a,b,x:c)

        tr13Helper f g h x y = let
                                  a = simplifySum [g x, g y]
                                  b = h $ simplifySum [x,y]
                               in
                                  1 `f` (a * b)

        tr13' (Mul xs) = let
                            (a,b,c) = partition xs
                            a' = tr13'' a
                            b' = tr13'' b
                            c' = return $ Mul c
                         in 
                            a' * b' * c'
        tr13' x = return x

        tr13'' [] = 1
        tr13'' [x] = return x
        tr13'' (Tan x:Tan y:xs) = tr13Helper (-) Tan (recip . tan) x y * tr13'' xs
        tr13'' (Cot x:Cot y:xs) = tr13Helper (+) Cot cot x y * tr13'' xs
        tr13'' xs = return $ Mul xs

trigFuns :: PExpr -> Int
trigFuns (Sin x) = trigFuns x + 1
trigFuns (Cos x) = trigFuns x + 1
trigFuns (Tan x) = trigFuns x + 1
trigFuns (Sec x) = trigFuns x + 1
trigFuns (Csc x) = trigFuns x + 1
trigFuns (Cot x) = trigFuns x + 1
trigFuns x = (sum . map trigFuns . operands) x

operations :: PExpr -> Int
operations (Add []) = 0
operations (Add xs) = sum (map operations xs) + length xs - 1
operations (Mul []) = 0
operations (Mul xs) = sum (map operations xs) + length xs - 1
operations (Pow x y) = operations x + operations y + 1
operations (Fun _ xs) = sum (map operations xs) + 1
operations _ = 0

measure :: PExpr -> (Int, Int)
measure x = (trigFuns x, operations x)

addStepPExpr :: String -> PExpr -> EvalSteps PExpr
addStepPExpr msg x = addStep msg >> return x

chain :: (Foldable t, Monad m) => t (a -> m a) -> (a -> m a)
chain = foldl (>=>) return

choice :: Monad m => [PExpr -> m PExpr] -> PExpr -> m PExpr
choice fs x = minimumBy (compare `on` measure) <$> mapM (\f -> f x) fs

ctr1 :: PExpr -> Expr
ctr1 = choice [chain [tr5,tr0], chain [tr6,tr0], return]

ctr2 :: PExpr -> Expr
ctr2 = choice [tr11, choice [chain [tr5,tr0], chain [tr6,tr0], tr0]]

ctr3 :: PExpr -> Expr
ctr3 = choice [chain [tr8, tr0], chain [tr8, tr10i, tr0], return]

ctr4 :: PExpr -> Expr
ctr4 = choice [chain [tr4, tr10i], return] 

rl1 :: PExpr -> Expr
rl1 = chain [tr4,tr3,tr4,tr12,tr4,tr13,tr4,tr0]

rl2 :: PExpr -> Expr
rl2 = choice [ chain [tr4,tr3,tr10,tr4,tr3,tr11], 
               chain [tr5,tr7,tr11,tr4],
               chain [ctr3, ctr1, tr9, ctr2, tr4, tr9, tr9, ctr4]
            ]

exists :: [String] -> PExpr -> Bool
exists symbols (Fun f _) = f `elem` symbols
exists symbols x = any (exists symbols) (operands x)

whenExists :: Monad m => [String] -> (PExpr -> m PExpr) -> PExpr -> m PExpr
whenExists symbols f x = if exists symbols x then f x else return x

fu :: Expr -> Expr
fu x = x 
        >>=
       automaticSymplify
        >>= 
       tr1 
        >>=
       whenExists ["Tan", "Cot"] (choice [rl1, return] >=> whenExists ["Tan", "Cot"] tr2) 
        >>= 
       whenExists ["Sin", "Cos"] (choice [rl2, const x, tr8, return])
        >>=
       choice [tr2i, return]