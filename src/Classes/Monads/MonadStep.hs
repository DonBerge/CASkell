{-# LANGUAGE TupleSections #-}
module MonadStep where

import Control.Monad

import Control.Applicative

import Classes.StepTree
import Data.Bifunctor (Bifunctor(second))

newtype StepT m a = StepT { runStepT :: m (StepTree, a) }

instance Monad m => Functor (StepT m) where
    fmap f = StepT . fmap (second f) . runStepT

instance Monad m => Applicative (StepT m) where
    pure = StepT . return . (E,)
    (<*>) = ap

instance Monad m => Monad (StepT m) where
    return = pure
    m >>= k = StepT $ do
        (_, a) <- runStepT m
        (step', b) <- runStepT (k a)
        return (step', b) 