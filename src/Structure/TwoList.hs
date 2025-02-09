{-|
  Module      : TwoList
  Description : Una estructura de datos para representar listas con al menos 2 elementos
-}

module TwoList (
  TwoList(..),
  NonEmpty( (:|) ),
)
where

import Data.List.NonEmpty

data TwoList a = a :|| NonEmpty a
  deriving (Eq, Show)

infixr 5 :||

instance Functor TwoList where
  fmap f (x :|| xs) = f x :|| fmap f xs

instance Foldable TwoList where
  foldMap f (x :|| xs) = f x <> foldMap f xs

instance Traversable TwoList where
  traverse f (x :|| xs) = (:||) <$> f x <*> traverse f xs

instance Semigroup (TwoList a) where
  (x :|| xs) <> (y :|| ys) = x :|| (xs <> (y :| []) <> ys)

