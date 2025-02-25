{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-| 
    Module: EvalSteps 
    Description: Monadaa 'EvalSteps', usada para computaciones que pueden fallar.
-}
module Classes.EvalSteps where

import Control.Monad.Except
import Control.Applicative
import Data.Either (isLeft)

-- | Alias de tipo para mensajes de error.
type Error = String

-- | El tipo 'EvalSteps' encapsula una computación que puede resultar en un error o en un valor.
newtype EvalSteps a = EvalSteps { unEvalSteps :: Except Error a }

runEvalSteps :: EvalSteps a -> Either Error a
runEvalSteps = runExcept . unEvalSteps


-- | Instancia de igualdad para 'EvalSteps'.
instance Eq a => Eq (EvalSteps a) where
    a == b = case (runEvalSteps a,runEvalSteps b) of
                (Left _, Left _) -> True
                (Right x, Right y) -> x == y
                (_,_) -> False

-- | Instancia de 'Functor' para 'EvalSteps'.
instance Functor EvalSteps where
    fmap f = EvalSteps . fmap f . unEvalSteps

-- | Instancia de 'Applicative' para 'EvalSteps'.
instance Applicative EvalSteps where
    pure = EvalSteps . return
    (<*>) f = EvalSteps . (unEvalSteps f <*>) . unEvalSteps

-- | Instancia de 'Monad' para 'EvalSteps'.
instance Monad EvalSteps where
    return = pure
    x >>= f = EvalSteps $ unEvalSteps x >>= unEvalSteps . f

-- | Instancia de 'MonadError' para 'EvalSteps'.
instance MonadError Error EvalSteps where
    throwError = EvalSteps . throwError
    catchError x f = EvalSteps $ catchError (unEvalSteps x) (unEvalSteps . f)

-- | Instancia de 'MonadFail' para 'EvalSteps'.
instance MonadFail EvalSteps where
    fail = throwError

-- | Instancia de 'Alternative' para 'EvalSteps'.
instance Alternative EvalSteps where
    empty = fail "Undefined value"
    a <|> b = case runEvalSteps a of
                Left _ -> b
                _ -> a

-- | Instancia de 'Show' para 'EvalSteps'.
instance Show a => Show (EvalSteps a) where
    show x = case runEvalSteps x of
                Left e -> "Undefined: " ++ e
                Right e -> show e

isUndefined :: EvalSteps a -> Bool
isUndefined = isLeft . runEvalSteps
