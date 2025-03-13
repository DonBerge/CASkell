{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-| 
    Module: EvalResult 
    Description: Monada 'EvalResult', usada para computaciones que pueden fallar.
-}
module Classes.EvalResult where

import Control.Applicative
import Data.Either (isLeft)

-- | Alias de tipo para mensajes de error.
type Error = String

-- | El tipo 'EvalResult' encapsula una computación que puede resultar en un error o en un valor.
newtype EvalResult a = EvalResult { runEvalResult :: Either Error a }


-- | Instancia de igualdad para 'EvalResult'.
instance Eq a => Eq (EvalResult a) where
    a == b = case (runEvalResult a,runEvalResult b) of
                (Left _, Left _) -> True
                (Right x, Right y) -> x == y
                (_,_) -> False

-- | Instancia de 'Functor' para 'EvalResult'.
instance Functor EvalResult where
    fmap f = EvalResult . fmap f . runEvalResult

-- | Instancia de 'Applicative' para 'EvalResult'.
instance Applicative EvalResult where
    pure = EvalResult . return
    (<*>) f = EvalResult . (runEvalResult f <*>) . runEvalResult

-- | Instancia de 'Monad' para 'EvalResult'.
instance Monad EvalResult where
    return = pure
    x >>= f = EvalResult $ runEvalResult x >>= runEvalResult . f

-- | Instancia de 'MonadFail' para 'EvalResult'.
instance MonadFail EvalResult where
    fail = EvalResult . Left

-- | Instancia de 'Alternative' para 'EvalResult'.
instance Alternative EvalResult where
    empty = fail "Undefined value"
    a <|> b = case runEvalResult a of
                Left _ -> b
                _ -> a

-- | Instancia de 'Show' para 'EvalResult'.
-- instance Show a => Show (EvalResult a) where
--     show x = case runEvalResult x of
--                 Left e -> "Undefined: " ++ e
--                 Right e -> show e

isUndefined :: EvalResult a -> Bool
isUndefined = isLeft . runEvalResult
