{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module TriBool where

data TriBool = F | U | T
  deriving (Eq, Ord, Show)

isTrue :: TriBool -> Bool
isTrue T = True
isTrue _ = False

isFalse :: TriBool -> Bool
isFalse F = True
isFalse _ = False

isUnknown :: TriBool -> Bool
isUnknown U = True
isUnknown _ = False

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