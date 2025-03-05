{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
  Module      : Data.TwoList
  Description : Una estructura de datos para representar listas con al menos 2 elementos

  Una TwoList es una lista que contiene al menos 2 elementos, pero que por lo demás es idéntica a la lista tradicional en complejidad y en términos de API.
-}

module Data.TwoList (
  TwoList(..),
  NonEmpty( (:|) ),
  toList,
  fromList,
  sort,
  sortBy,
  intersperse,
  intercalate,
  reverse,
  partition
)
where

import Prelude hiding (reverse)

import Data.List.NonEmpty ( NonEmpty(..), (<|) ) 
import qualified Data.List.NonEmpty as NE

-- | Una lista con al menos 2 elementos
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
  (x :|| xs) <> (y :|| ys) = x :|| (xs <> (y <| ys))

-- | Convierte una 'TwoList' en una lista
toList :: TwoList a -> [a]
toList (x :|| xs) = x : NE.toList xs

-- | Convierte una lista a una 'TwoLIst'
fromList :: [a] -> Maybe (TwoList a)
fromList [] = Nothing
fromList [_] = Nothing
fromList (x:xs) = Just (x :|| NE.fromList xs)

-- | Convierte una función que opera en listas no vacías en una función que opera en TwoLists
lift :: (NonEmpty a -> NonEmpty b) -> (TwoList a -> TwoList b)
lift f (x :|| xs) = let (x' :| y' : ys') = f (x <| xs) in x' :|| y' :| ys'

-- | Ordena una 'TwoList'
sort :: Ord a => TwoList a -> TwoList a
sort = lift NE.sort

-- | Ordena una 'TwoList' usando una función de comparación
sortBy :: (a -> a -> Ordering) -> TwoList a -> TwoList a
sortBy = lift . NE.sortBy

-- | Inserta un elemento entre cada par de elementos de una 'TwoList'
intersperse :: a -> TwoList a -> TwoList a
intersperse x = lift (NE.intersperse x)

-- | Intercala una lista entre cada par de elementos de una 'TwoList'
intercalate :: [a] -> TwoList [a] -> [a]
intercalate x = concat . intersperse x

-- | Invierte una 'TwoList'
reverse :: TwoList a -> TwoList a
reverse = lift NE.reverse

-- | Particiona una 'TwoList' en dos listas, una con los elementos que cumplen un predicado y otra con los que no
partition :: (a -> Bool) -> TwoList a -> ([a], [a])
partition p (x :|| xs) = NE.partition p (x <| xs)