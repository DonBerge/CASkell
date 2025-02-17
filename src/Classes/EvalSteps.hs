{-| 
    Module: EvalSteps 
    Description: Monadaa 'EvalSteps', usada para computaciones que pueden fallar y que ademas llevan un registro de mensajes.
-}
module Classes.EvalSteps where

import Control.Applicative
import Data.Bifunctor
import Data.Either

-- | Alias de tipo para mensajes de error.
type Error = String

-- | El tipo 'EvalSteps' encapsula una computación que puede resultar en un error o en un valor,
-- junto con una lista de mensajes de registro.
newtype EvalSteps a = EvalSteps { unEvalSteps :: (Either Error a, [String]) }

-- | Instancia de igualdad para 'EvalSteps'.
instance Eq a => Eq (EvalSteps a) where
    EvalSteps (Left _, _) == EvalSteps (Left _, _) = True
    EvalSteps (Right x, _) == EvalSteps (Right y, _) = x == y
    _ == _ = False

-- | Instancia de 'Functor' para 'EvalSteps'.
instance Functor EvalSteps where
    fmap f (EvalSteps x) = EvalSteps $ first (fmap f) x

-- | Instancia de 'Applicative' para 'EvalSteps'.
instance Applicative EvalSteps where
    pure x = EvalSteps (return x, [])
    EvalSteps (Left e, logs1) <*> _ = EvalSteps (Left e, logs1)
    _ <*> EvalSteps (Left e, logs2) = EvalSteps (Left e, logs2)
    EvalSteps (Right f, logs1) <*> EvalSteps (Right x, logs2) = EvalSteps (Right $ f x, logs1 ++ logs2)

-- | Instancia de 'Monad' para 'EvalSteps'.
instance Monad EvalSteps where
    return = pure
    EvalSteps (Left e, logs1) >>= _ = EvalSteps (Left e, logs1)
    EvalSteps (Right x, logs1) >>= f = let EvalSteps (y, logs2) = f x in EvalSteps (y, logs1 ++ logs2)

-- | Instancia de 'MonadFail' para 'EvalSteps'.
instance MonadFail EvalSteps where
    fail e = EvalSteps (Left e, [])

-- | Instancia de 'Alternative' para 'EvalSteps'.
instance Alternative EvalSteps where
    empty = fail "Undefined value"
    EvalSteps (Left _, _) <|> b = b
    a <|> _ = a

-- | Instancia de 'Show' para 'EvalSteps'.
instance Show a => Show (EvalSteps a) where
    show (EvalSteps (x, [])) = case x of
                                    Left e -> "Undefined: " ++ e
                                    Right a -> show a
    show (EvalSteps (x, logs)) = unlines logs ++ "\n" ++ show (EvalSteps (x, []))

isUndefined :: EvalSteps a -> Bool
isUndefined = isLeft . fst . unEvalSteps

-- | Función para agregar un mensaje de traza.
-- Agrega un mensaje de registro a la computación 'EvalSteps'.
addStep :: String -> EvalSteps ()
addStep msg = EvalSteps (return (), [msg])