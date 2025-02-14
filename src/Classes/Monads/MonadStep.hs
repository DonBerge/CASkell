module MonadStep where

import Control.Monad

import Control.Applicative

import Classes.StepTree

newtype StepT m a = StepT { runStepT :: m (StepTree, a) }

instance Monad m => Functor (StepT m) where
    fmap f (StepT m) = StepT $ do
        (step, a) <- m
        return (step, f a)

instance Monad m => Applicative (StepT m) where
    pure a = StepT $ return (E, a)
    (<*>) = ap
 
addChildren :: StepTree -> StepTree -> StepTree
addChildren E t = t
addChildren t E = t
addChildren (Step n o r c) t = Step n o r (c ++ [t])

instance Monad m => Monad (StepT m) where
    return = pure
    m >>= k = StepT $ do
        (step, a) <- runStepT m
        (step', b) <- runStepT (k a)
        return (addChildren step' step, b)