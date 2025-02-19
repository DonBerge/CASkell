{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Classes.Monads.MonadAssumptions where

import TriBool
import Control.Monad.Reader

data AssumptionsEnviroment = AssumptionsEnviroment {
    askPositive :: TriBool,
    askNegative :: TriBool,
    askZero :: TriBool,
    askEven :: TriBool,
    askOdd :: TriBool,
    askInteger :: TriBool
}

emptyAssumptions :: AssumptionsEnviroment
emptyAssumptions = AssumptionsEnviroment U U U U U U

type AssumptionsT = ReaderT AssumptionsEnviroment

instance Show (m a) => Show (AssumptionsT m a) where
    show = show . runAssumptionsT

runAssumptionsT :: AssumptionsT m a -> m a
runAssumptionsT = flip runReaderT emptyAssumptions