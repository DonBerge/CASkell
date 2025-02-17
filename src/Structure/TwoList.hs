{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
  Module      : TwoList
  Description : Una estructura de datos para representar listas con al menos 2 elementos
-}

module TwoList (
  TwoList(..),
  NonEmpty( (:|) ),
  toList,
  sort,
  sortBy,
  intersperse,
  intercalate,
  reverse
)
where

import Prelude hiding (reverse)

import Data.List.NonEmpty ( NonEmpty(..) ) 
import qualified Data.List.NonEmpty as NE

data TwoList a = a :|| NonEmpty a
  deriving (Eq, Show)

infixr 5 :||

instance Functor TwoList where
  fmap f = lift (fmap f)

instance Foldable TwoList where
  foldMap f (x :|| xs) = f x <> foldMap f xs

instance Traversable TwoList where
  traverse f (x :|| xs) = (:||) <$> f x <*> traverse f xs

instance Semigroup (TwoList a) where
  (x :|| xs) <> (y :|| ys) = x :|| (xs <> (y :| []) <> ys)

toList :: TwoList a -> [a]
toList (x :|| xs) = x : NE.toList xs

lift :: (NonEmpty a1 -> NonEmpty a2) -> TwoList a1 -> TwoList a2
lift f (x :|| xs) = let (x' :| y' : ys') = f (x :| NE.toList xs) in x' :|| y' :| ys'

sort :: Ord a => TwoList a -> TwoList a
sort = lift NE.sort

sortBy :: (a -> a -> Ordering) -> TwoList a -> TwoList a
sortBy = lift . NE.sortBy

intersperse :: a -> TwoList a -> TwoList a
intersperse x = lift (NE.intersperse x)

intercalate :: [a] -> TwoList [a] -> [a]
intercalate x = concat . intersperse x

reverse :: TwoList a -> TwoList a
reverse = lift NE.reverse