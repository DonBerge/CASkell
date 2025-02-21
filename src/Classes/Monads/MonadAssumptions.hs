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

setPositive :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setPositive p env = env { askPositive = p }

setNegative :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setNegative n env = env { askNegative = n }

setZero :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setZero z env = env { askZero = z }

setEven :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setEven e env = env { askEven = e }

setOdd :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setOdd o env = env { askOdd = o }

setInteger :: TriBool -> AssumptionsEnviroment -> AssumptionsEnviroment
setInteger i env = env { askInteger = i }


emptyAssumptions :: AssumptionsEnviroment
emptyAssumptions = AssumptionsEnviroment U U U U U U

type AssumptionsT = ReaderT AssumptionsEnviroment

instance Show (m a) => Show (AssumptionsT m a) where
    show = show . runAssumptionsT

runAssumptionsT :: AssumptionsT m a -> m a
runAssumptionsT = flip runReaderT emptyAssumptions