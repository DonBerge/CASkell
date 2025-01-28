{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Calculus where

import Expr
import qualified Calculus.Derivate as D
import qualified Calculus.Integrate as I

derivate :: Expr -> Expr -> Expr
derivate u x = do
                u' <- u
                x' <- x
                D.derivate u' x'

derivateMultiple :: Expr -> Expr -> Int -> Expr
derivateMultiple u x n = do
                            u' <- u
                            x' <- x
                            loopDeriv u' x' n
    where
        loopDeriv u _ 0 = return u
        loopDeriv u x n = do
                            dx <- D.derivate u x
                            loopDeriv dx x (n-1)

integrate :: Expr -> Expr -> Expr
integrate u x = do
                  u' <- u
                  x' <- x
                  I.integrate u' x'