{-|
  Module      : TwoList
  Description : Una estructura de datos para representar listas con al menos 2 elementos
-}

module TwoList (
  TwoList(..),
  NonEmpty( (:|) ),
  toList
)
where

import Data.List.NonEmpty ( NonEmpty(..) ) 
import qualified Data.List.NonEmpty as NE

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

toList :: TwoList a -> [a]
toList (x :|| xs) = x : NE.toList xs