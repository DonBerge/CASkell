module Simplification.Algebraic (
    expand
) where

import Expr
import PExpr

import Symplify

import qualified Number as N

import Math.Combinatorics.Exact.Binomial (choose)

import Classes.Assumptions
import TriBool

expr :: PExpr -> Expr
expr = return 

numerator :: PExpr -> Expr
numerator (Number n) = fromInteger $ N.numerator n
numerator (Add []) = numerator 0
numerator (Mul []) = numerator 1
numerator (Mul xs) = product $ map numerator xs
numerator (Pow _ y)
    | isTrue $ isNegative y = 1
numerator (Exp x)
    | isTrue $ isNegative x = 1
numerator x = expr x    

denominator :: PExpr -> Expr
denominator (Number n) = fromInteger $ N.denominator n
denominator (Add []) = denominator 0
denominator (Mul []) = denominator 1
denominator (Mul xs) = product $ map denominator xs
denominator u@(Pow _ y)
    | isTrue $ isNegative y = recip $ expr u
denominator (Exp x)
    | isTrue $ isNegative x = exp $ negate $ expr x
denominator _ = 1

expand :: Expr -> Expr
expand x = x >>= expand'

expand' :: PExpr -> Expr
expand' (Add []) = 0
expand' (Add [x]) = expand' x >>= expandFraction
expand' (Add (x:xs)) = expand' x + expand' (Add xs) >>= expandFraction
                        

expand' (Mul []) = 1
expand' (Mul [x]) = expand' x >>= expandFraction
expand' (Mul (x:xs)) = do
                        x' <- expand' x
                        xs' <- expand' (Mul xs)
                        expandProduct x' xs'

expand' (Pow b (Number f))
    | f > 0 = let
                (fl, m) = properFraction f
              in do 
                    b' <- expand' b
                    t' <- if f == m then expr (Pow b (Number m)) else expand $ simplifyPow b' (Number m)
                    expandPower b' fl >>= expandProduct t'

expand' (Pow b e) = do
                        b' <- expand' b
                        e' <- expand' e
                        expandFraction $ Pow b' e'

expand' (Fun f xs) = (Fun f <$> mapM (expand') xs) >>= expandFraction

expand' u = expr u >>= expandFraction

expandFraction :: PExpr -> Expr
expandFraction x = do
                    d <- denominator x
                    if d == 1
                        then expr x
                        else numerator x / (expand $ denominator x)

expandProduct :: PExpr -> PExpr -> Expr
expandProduct (Add []) _ = 0
expandProduct (Add (r:rs)) s = expandProduct r s + expandProduct (Add rs) s >>= expand' -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
expandProduct r s@(Add _) = expandProduct s r
expandProduct r s = expr r * expr s >>= expandFraction

-- Se asume que n es no negativo
expandPower :: PExpr -> Integer -> Expr
expandPower _ 0 = 1
expandPower u 1 = expr u
expandPower 0 _ = 0
expandPower (Add []) _ = 0
expandPower (Add (f:rs)) n = sum [ do
                                    cf' <- cf k
                                    cr' <- expandPower (Add rs) k
                                    expandProduct cf' cr'  | k <- [0..n] ] >>= expandFraction -- autosimplificacion puede generar terminos no expanadidos, por lo que hay que expnadir otra vez
    where
        coeff:: Integer -> Expr
        coeff k = fromInteger (choose n k)
        
        expo :: Integer -> Expr
        expo k = fromInteger (n-k)

        cf :: Integer -> Expr
        cf k = coeff k * expr f ** expo k
expandPower u n = (expr $ Pow u $ fromInteger n) >>= expandFraction