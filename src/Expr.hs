{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Prelude hiding (const, exponent)

import PExpr

import Symplify

import qualified Number as N

newtype Fail a = Fail { unFail :: Maybe a } deriving (Ord, Eq)

instance Functor Fail where
    fmap f (Fail x) = Fail $ f <$> x

instance Applicative Fail where
    pure = Fail . Just
    Fail f <*> Fail x = Fail $ f <*> x

instance Monad Fail where
    return = pure
    
    Fail Nothing >>= _ = Fail Nothing
    Fail (Just x) >>= f = f x

instance MonadFail Fail where
    fail _ = Fail Nothing


instance Show x => Show (Fail x) where
    show (Fail Nothing) = "Undefined"
    show (Fail (Just x)) = show x

type Expr = Fail PExpr

instance Num Expr where
    fromInteger = return . Number . fromInteger
    p + q = do
              p' <- p
              q' <- q
              Add xs <- return $ p' + q'
              simplifySum xs
    p * q = do
              p' <- p
              q' <- q
              Mul xs <- return $ p' * q'
              simplifyProduct xs

    negate p = p >>= simplifyProduct . (:[Number (-1)])

    p - q = do
              p' <- p
              q' <- negate q
              Add xs <- return $ p' + q'
              simplifySum xs

    abs x
        | isTrue $ isNegative x = negate x
        | isTrue $ isPositive x = x
        | otherwise = fail "abs is undefined for this expression"
    
    signum = undefined
        
instance Fractional Expr where
    fromRational = return . Number . fromRational
    p / q = do
              p' <- p
              q' <- q >>= (`simplifyPow` (-1))
              Mul xs <- return $ p' * q'
              simplifyProduct xs

makeFun :: (PExpr -> PExpr) -> Expr -> Expr
makeFun f x = Fail $ f <$> unFail x 

---
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
freeOf u t = all (freeOf t) $ operands u


linearForm :: PExpr -> PExpr -> Fail (PExpr, PExpr)
linearForm u x
    | u == x = return (1, 0)
    | notASymbol x = fail "x must be a symbol"
        where
            notASymbol (Symbol _) = False
            notASymbol _ = True
linearForm u@(Number _) _ = return (0, u)
linearForm u@(Symbol _) _ = return (0, u)
linearForm u@(Mul _) x
    | freeOf u x = return (0, u)
    | otherwise = do
                    udivx <- (return u) / (return x)
                    if freeOf udivx x
                        then return (0, u)
                        else fail "not a linear form"
linearForm u@(Add []) _ = return (0, u)
linearForm (Add (u:us)) x = do
                                (a,b) <- linearForm u x
                                (c,d) <- linearForm (Add us) x
                                a' <- simplifySum [a, c]
                                b' <- simplifySum [b, d]
                                return (a', b')
linearForm u x
    | freeOf u x = return (0, u)
    | otherwise = fail "not a linear form"

---


instance Floating Expr where
    pi = return Pi
    exp = makeFun Exp
    log = makeFun Log

    sin x = do
                (a,b) <- x >>= (`linearForm` Pi)
                -- x = a * pi + b
                case a of
                    1 -> case b of
                            Mul cs | foldl (\c -> (c /=) . (isTrue . isNegative)) False cs -> Sin <$> negate (return b)
                            _ -> negate $ return $ Sin b
                                    
                    _ -> makeFun Sin x

    cos 0 = 1
    cos x = makeFun Cos x
    
    tan = makeFun Tan
    asin = makeFun Asin
    acos = makeFun Acos
    atan = makeFun Atan
    sinh = makeFun Sinh
    cosh = makeFun Cosh
    tanh = makeFun Tanh
    asinh = makeFun Asinh
    acosh = makeFun Acosh
    atanh = makeFun Atanh

    sqrt x = x ** 0.5

    p ** q =  do
                p' <- p
                q' <- q
                simplifyPow p' q'

sec :: Expr -> Expr
sec = makeFun Sec

csc :: Expr -> Expr
csc = makeFun Csc

cot :: Expr -> Expr
cot = makeFun Cot

---------

extractTriBool :: Fail TriBool -> TriBool
extractTriBool (Fail Nothing) = U
extractTriBool (Fail (Just x)) = x

instance Assumptions Expr where
    isNegative = extractTriBool . fmap isNegative
    isPositive = extractTriBool . fmap isPositive
    isZero = extractTriBool . fmap isZero
    isEven = extractTriBool . fmap isEven
    isOdd = extractTriBool . fmap isOdd
    isInteger = extractTriBool . fmap isInteger
                        



--------

numerator :: PExpr -> Expr
numerator (Number n) = fromInteger $ N.numerator n
numerator (Add []) = numerator 0
numerator (Mul []) = numerator 1
numerator (Mul xs) = product $ map numerator xs
numerator (Pow _ y)
    | isTrue $ isNegative y = 1
numerator (Exp x)
    | isTrue $ isNegative x = 1
numerator x = return x    

denominator :: PExpr -> Expr
denominator (Number n) = fromInteger $ N.denominator n
denominator (Add []) = denominator 0
denominator (Mul []) = denominator 1
denominator (Mul xs) = product $ map denominator xs
denominator u@(Pow _ y)
    | isTrue $ isNegative y = recip $ return u
denominator (Exp x)
    | isTrue $ isNegative x = exp $ negate $ return x
denominator _ = 1


-------

number :: Rational -> Expr
number = fromRational

symbol :: String -> Expr
symbol = pure . Symbol