{-# LANGUAGE FunctionalDependencies #-}

module TriBool where

data TriBool = F | U | T
  deriving (Eq, Ord, Show)

true :: TriBool -> Bool
true T = True
true _ = False

false :: TriBool -> Bool
false F = True
false _ = False

unknown :: TriBool -> Bool
unknown U = True
unknown _ = False

---

liftBool :: Bool -> TriBool
liftBool True = T
liftBool False = F

class BAlgebra a b c | a b -> c where
    (&&&) :: a -> b -> c
    (|||) :: a -> b -> c
    (!&&)  :: a -> b -> c -- nand
    (/||) :: a -> b -> c -- xor
    
not3 :: BAlgebra b b c => b -> c
not3 p = p !&& p

instance BAlgebra Bool Bool Bool where
    (&&&) = (&&)
    (|||) = (||)
    (!&&) = (&&) . not 
    (/||) = (/=)

instance BAlgebra TriBool TriBool TriBool where
    F &&& _ = F
    _ &&& F = F
    T &&& T = T
    _ &&& _ = U

    T ||| _ = T
    _ ||| T = T
    F ||| F = F
    _ ||| _ = U

    F !&& _ = T
    _ !&& F = T
    T !&& T = F
    _ !&& _ = U

    U /|| _ = U
    _ /|| U = U
    p /|| q = liftBool $ p /= q

instance BAlgebra Bool TriBool TriBool where
    (&&&) = (&&&) . liftBool
    (|||) = (|||) . liftBool
    (!&&) = (!&&) . liftBool
    (/||) = (/||) . liftBool

instance BAlgebra TriBool Bool TriBool where
    (&&&) = flip (&&&)
    (|||) = flip (|||)
    (!&&) = flip (!&&)
    (/||) = flip (/||)

xor3 :: (a -> TriBool) -> [a] -> TriBool
xor3 _ [] = F
xor3 f (x:xs)
    | unknown (f x) = U
    | false (f x) = xor3 f xs
    | otherwise = xor3 f xs

and3 :: (a -> TriBool) -> [a] -> TriBool
and3 _ [] = T
and3 f (x:xs)
    | unknown (f x) = U
    | false (f x) = F
    | otherwise = and3 f xs

or3 :: (a -> TriBool) -> [a] -> TriBool
or3 _ [] = T
or3 f (x:xs)
    | unknown (f x) = U
    | true (f x) = T
    | otherwise = or3 f xs

triBoolCase :: TriBool -> a -> a -> a -> a
triBoolCase T t _ _ = t
triBoolCase F _ f _ = f
triBoolCase U _ _ u = u