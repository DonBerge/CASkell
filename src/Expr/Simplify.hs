{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- |
--    Module: AutoSimplify
--
--    Description: Funciones usadas en el proceso de autosimplificación
--
--    El proceso de autosimplificación se define como una colección de transformaciones algebraicas
--    y de simplificación de funciones que se aplican a una expresión durante la evaluación.
--
--    Autosimplificación es un acronimo de "Simplificación automatica", y se llama de esta manera ya que
--    las transformaciones se realizan automaticamente sin la intervención directa del usuario.
--
--    Por ejemplo, la expresión @2 * 3 + 4@ es autosimplificada a @10@ y @x+x@ autosimplifica a @2*x@.
--
--    La autosimplificación no realiza todas las simplificaciones posibles, por ejemplo, la expresión
--    @a*x+b*x+2*x@ no se simplifica a @(a+b+2)*x@, ya que simplificaciones de este tipo colisionan con
--    funciones como 'expand'.
module Expr.Simplify
  ( module Classes.EvalResult,
    module PExpr,
    -- * Función de autosimplificación
    automaticSymplify,
    -- * Autosimplificación de potencias
    simplifyPow,
    simplifyIntPow,
    simplifySqrt,
    -- * Autosimplificación de productos
    simplifyNegate,
    simplifyProduct,
    simplifyDiv,
    -- * Autosimplificación de sumas
    simplifySum,
    simplifySub,
    -- * Autosimplificación de funciones
    simplifyFun,
    -- * Pattern matching y manipulación de la estructura
    operands,
    mulByNeg,
    freeOf,
    linearForm,
  )
where

import Classes.EvalResult
import Control.Applicative
import Data.List
import Data.Number (Number)
import PExpr
import Prelude hiding (const, exponent)

-- * Función de autosimplificación

-- | Autosimplificación de arboles de expresiones
automaticSymplify :: PExpr -> EvalResult PExpr
automaticSymplify (Mul xs) = mapM automaticSymplify xs >>= simplifyProduct
automaticSymplify (Add xs) = mapM automaticSymplify xs >>= simplifySum
automaticSymplify (Pow x y) = do
  x' <- automaticSymplify x
  y' <- automaticSymplify y
  simplifyPow x' y'
automaticSymplify (Fun f xs) = do
  xs' <- mapM automaticSymplify xs
  simplifyFun $ Fun f xs'
automaticSymplify x = return x

------------------------------

-- | Verificación de constantes
isConstant :: PExpr -> Bool
isConstant (Number _) = True
-- isConstant Pi = True
isConstant _ = False

-- | Extrae la parte constante de una multiplicación
const :: PExpr -> PExpr
const (Mul []) = Number 1
const (Mul (x : _))
  | isConstant x = x
  | otherwise = Number 1
const u
  | isConstant u = error "Constants dont have terms"
  | otherwise = Number 1

-- | Extrae la parte variable de una multiplicación
term :: PExpr -> PExpr
term (Mul []) = error "Empty products do not have terms"
term (Mul (x : xs))
  | isConstant x = Mul xs
  | otherwise = Mul (x : xs)
term u
  | isConstant u = error "Constants dont have terms"
  | otherwise = Mul [u]

-- | Extrae la base de una potencia
base :: PExpr -> PExpr
base (Number _) = error "Base of a number is not defined"
base (Pow x _) = x
base u = u

-- | Extrae el exponente de una potencia
exponent :: PExpr -> PExpr
exponent (Number _) = error "Exponent of a number is not defined"
exponent (Pow _ x) = x
exponent _ = Number 1

---------------------------------------------------------------------------------------


-- * Autosimplificación de potencias

-- |
--    Simplificación de potencias
--
--    Sea @u = Pow v w@ la expresión a simplificar, entonces las reglas de simplificación son las siguientes:
--
--    [@SPOW-1@]: Si @v = Undefined@ o @w = Undefined@, entonces @'simplifyPow' u = Undefined@
--
--    [@SPOW-2@]: Si @v = 0@:
--
--        * Si @w > 0@, entonces @'simplifyPow' u = 0@
--        * Si @w <= 0@, entonces @'simplifyPow' u = Undefined@
--     Si no se puede determinar el signo de @w@ esta regla no determina nada. Se continua con las siguientes
--
--    [@SPOW-3@]: Si @v=1@ entonces @'simplifyPow' u = 1@
--
--    [@SPOW-4@]: Si @w@ es entero, entonces @'simplifyPow' u = 'simplifyIntPow' u w@
--
--    [@SPOW-5@]: Si ninguna regla aplica, entonces @'simplifyPow' u = u@
--
--    === Ejemplos
simplifyPow :: PExpr -> PExpr -> EvalResult PExpr
-- SPOW-1 es manejada automaticamente por la monada EvalResult
simplifyPow (Number 0) w -- SPOW-2
  | true $ isPositive w = return (Number 0) -- SPOW-2.1
  | true (isNegative w ||| isZero w) = fail "Division por cero" -- SPOW-2.2
simplifyPow (Number 1) _ = return (Number 1) -- SPOW-3
simplifyPow (Number n) (Number m)
  | true (isInteger (n**m)) = return $ Number $ n**m -- si el resultado es entero, evaluar en los numeros
simplifyPow v w
  | true $ isInteger w = simplifyIntPow v w -- SPOW.4
  | otherwise = return (Pow v w) -- SPOW.5

-- |
--    Autosimplificación de potencias con exponentes enteros
--
--    Sea la expresión @v^n@ con @v/=0@ y @n@ entero, 'simplifyIntPow' se define con las siguientes reglas:
--
--    [@SINTPOW-1@]: Si @v = Number x@ y @n = Number y@, entonces @'simplifyIntPow' v n = Number (x^y)@
--
--    [@SINTPOW-2@]: Si @n = 0@, entonces @'simplifyIntPow' v n = 1@
--
--    [@SINTPOW-3@]: Si @n = 1@, entonces @'simplifyIntPow' v n = v@
--
--    [@SINTPOW-4@]: Si @v = Pow r s@, sea p = 'simplifyProduct' [s,n]:
--
--        * Si @p@ es entero, entonces @'simplifyIntPow' v n = 'simplifyIntPow' r p@
--        * Si no, entonces @'simplifyIntPow' v n = Pow r p@
--
--    [@SINTPOW-5@]: Si @v = Mul rs@, entonces @'simplifyIntPow' v n = 'simplifyProduct' [u^n | u <- r]@
--
--    [@SINTPOW-6@]: Si ninguna regla aplica, entonces @'simplifyIntPow' v n = Pow v n@
simplifyIntPow :: PExpr -> PExpr -> EvalResult PExpr
simplifyIntPow (Number x) (Number n) = return $ Number $ x ^^ toInteger n -- SINTPOW-1
simplifyIntPow _ (Number 0) = return (Number 1) -- SINTPOW-2
simplifyIntPow x (Number 1) = return x -- SINTPOW-3
simplifyIntPow (Pow r s) n = do
  p <- simplifyProduct [s, n]
  if true (isInteger p)
    then simplifyIntPow r p -- SINTPOW-4.1
    else return $ Pow r p -- SINTPOW-4.2
simplifyIntPow (Mul rs) n = mapM (`simplifyIntPow` n) rs >>= simplifyProduct -- SINTPOW-5
simplifyIntPow x n = return $ Pow x n -- SINTPOW-6

{-|
    La autosimplificación de productos se basa en las siguientes transformaciones:

    1. Propiedad asociativa: \(a(bc) = (ab)c = abc\)
    2. Propiedad conmutativa: \(ab = ba\)
    3. Multiplicación de potencias de igual base: \(a^n a^m = a^{n+m}\)
    4. Propiedad cancelativa: \(0\cdot a = 0\)
    5. Existencia del neutro: \(1 \cdot a = a\)
    6. Multiplicación de números: @(Number a) * (Number b) = Number (a*b)@
    7. Productos con undefined son undefined: @a * undefined = undefined@
    8. Distribución de constantes en sumas: \(a(b+c) = ab + ac\)
-}
simplifyProduct :: [PExpr] -> EvalResult PExpr
simplifyProduct [] = return (Number 1)
simplifyProduct [x] = return x -- SPRD.3
simplifyProduct xs
  | Number 0 `elem` xs = return (Number 0) -- SPRD.2
  | otherwise = do
      xs' <- simplifyProductRec xs
      case xs' of -- SPRD.4
        [] -> return (Number 1)
        [x] -> return x
        [u, Add vs] | isConstant u -> Add . sort <$> mapM (simplifyProduct . reverse . (: [u])) vs
        _ -> return $ Mul $ sort xs'


-- SPRDREC-2
simplifyProductRec :: [PExpr] -> EvalResult [PExpr]
simplifyProductRec [] = return []
simplifyProductRec [Mul us, Mul vs] = mergeProducts us vs
simplifyProductRec [Mul us, v] = mergeProducts us [v]
simplifyProductRec [u, Mul vs] = mergeProducts [u] vs
---- SPRDREC-1
simplifyProductRec [Number u, Number v]
  | u * v == 1 = return []
  | otherwise = return [Number $ u * v]
simplifyProductRec [Number 1, v] = return [v]
simplifyProductRec [u, Number 1] = return [u]
simplifyProductRec [u, v]
  | v < u = simplifyProductRec [v, u]
  | isConstant u = return [u, v] -- evita un error en el caso de abajo
  | base u == base v = do
      s <- simplifySum [exponent u, exponent v]
      p <- simplifyPow (base u) s
      if p == Number 1
        then return []
        else return [p]
  | otherwise = return [u, v]
-- SPRDREC-3
simplifyProductRec ((Mul us) : vs) = simplifyProductRec vs >>= mergeProducts us
simplifyProductRec (u : vs) = simplifyProductRec vs >>= mergeProducts [u]

{-|
    Autosimplificación de divisiones

    Basada en esta propiedad: \( \dfrac{a}{b} = a \cdot b^{(-1)} \)
-}
simplifyDiv :: PExpr -> PExpr -> EvalResult PExpr
simplifyDiv x y = do
  y' <- simplifyPow y (Number (-1))
  simplifyProduct [x, y']

{-|
    La autosimplificación de productos se basa en las siguientes transformaciones:

    1. Suma de terminos semejantes: \(ax + bx = (a+b)x\)
    2. Propiedad asociativa: \(a+(b+c) = (a+b)+c = a+b+c\)
    3. Existencia del neutro: \(a+0 = a\)
    4. Propiedad conmutativa: \(a+b = b+a\)
    5. Suma de números: @Number a + Number b = Number (a+b)@
    6. Sumas con undefined son undefined: @a + undefined = undefined@
-}
simplifySum :: [PExpr] -> EvalResult PExpr
simplifySum [] = return (Number 0)
simplifySum [x] = return x
simplifySum xs = do
  xs' <- simplifySumRec xs
  case xs' of -- SPRD.4
    [] -> return (Number 0)
    [x] -> return x
    _ -> return $ Add $ sort xs'

simplifySumRec :: [PExpr] -> EvalResult [PExpr]
simplifySumRec [] = return []
simplifySumRec [Add us, Add vs] = mergeSums us vs
simplifySumRec [Add us, v] = mergeSums us [v]
simplifySumRec [u, Add vs] = mergeSums [u] vs
---- SPRDREC-1
simplifySumRec [Number u, Number v]
  | u + v == 0 = return []
  | otherwise = return [Number $ u + v]
simplifySumRec [Number 0, v] = return [v]
simplifySumRec [u, Number 0] = return [u]
simplifySumRec [u, v]
  | v < u = simplifySumRec [v, u]
  | isConstant u = return [u, v] -- evita undefined en el caso de abajo
  | term u == term v =
      let vt = term v
          uc = const u
          vc = const v
       in do
            s <- simplifySum [uc, vc]
            p <- simplifyProduct [s, vt]
            if p == Number 0
              then return []
              else return [p]
  | otherwise = return [u, v]
-- SPRDREC-3
simplifySumRec ((Add us) : vs) = simplifySumRec vs >>= mergeSums us
simplifySumRec (u : vs) = simplifySumRec vs >>= mergeSums [u]

{-|
    Autosimplificación de restas

    Basada en esta propiedad: \( a - b = a + (-1)\cdot b \)
-}
simplifySub :: PExpr -> PExpr -> EvalResult PExpr
simplifySub x y = do
  y' <- simplifyProduct [y, Number (-1)]
  simplifySum [x, y']

mergeOps :: (Monad m, Eq a) => ([a] -> m [a]) -> [a] -> [a] -> m [a]
mergeOps _ p [] = return p
mergeOps _ [] q = return q
mergeOps f (p : ps) (q : qs) = do
  h <- f [p, q]
  case h of
    [] -> mergeOps f ps qs
    [h'] -> (h' :) <$> mergeOps f ps qs
    [r, _] ->
      if p == r
        then (p :) <$> mergeOps f ps (q : qs)
        else (q :) <$> mergeOps f (p : ps) qs
    _ -> error "mergeOps: unexpected pattern"

mergeProducts :: [PExpr] -> [PExpr] -> EvalResult [PExpr]
mergeProducts = mergeOps simplifyProductRec

mergeSums :: [PExpr] -> [PExpr] -> EvalResult [PExpr]
mergeSums = mergeOps simplifySumRec

{-|
    Autosimplificacion de negaciones

    Basada en esta propiedad: \( -a = -1 \cdot a \)
-}
simplifyNegate :: PExpr -> EvalResult PExpr
simplifyNegate a = simplifyProduct [a, Number (-1)]

operands :: PExpr -> [PExpr]
operands (Add xs) = xs
operands (Mul xs) = xs
operands (Pow x y) = [x, y]
operands (Fun _ xs) = xs
operands _ = []

freeOf :: PExpr -> PExpr -> Bool
freeOf u t
  | u == t = False
freeOf (Symbol _) _ = True
freeOf (Number _) _ = True
freeOf u t = all (`freeOf` t) $ operands u

linearForm :: PExpr -> PExpr -> EvalResult (PExpr, PExpr)
linearForm u x
  | u == x = return (Number 1, Number 0)
  | notASymbol x = fail "x must be a symbol"
  where
    notASymbol (Symbol _) = False
    notASymbol _ = True
linearForm u@(Number _) _ = return (Number 0, u)
linearForm u@(Symbol _) _ = return (Number 0, u)
linearForm u@(Mul _) x
  | freeOf u x = return (Number 0, u)
  | otherwise = do
      udivx <- simplifyDiv u x
      if freeOf udivx x
        then return (udivx, Number 0)
        else fail "not a linear form"
linearForm u@(Add []) _ = return (Number 0, u)
linearForm (Add (u : us)) x = do
  (a, b) <- linearForm u x
  (c, d) <- linearForm (Add us) x
  a' <- simplifySum [a, c]
  b' <- simplifySum [b, d]
  return (a', b')
linearForm u x
  | freeOf u x = return (Number 0, u)
  | otherwise = fail "not a linear form"

mulByNeg :: PExpr -> Bool
mulByNeg (Number a) = a < 0
mulByNeg (Mul ((Number a) : _)) = a < 0
mulByNeg _ = False

simplifySqrt :: PExpr -> EvalResult PExpr
simplifySqrt (Number n)
  | n < 0 = fail "Raiz cuadrada de un número negativo"
simplifySqrt x = simplifyPow x (Number 0.5)
----------------

handlePeriod :: (Number -> PExpr -> EvalResult PExpr) -> (PExpr -> EvalResult PExpr) -> PExpr -> EvalResult PExpr
handlePeriod cases onOddPi x = do
  p <- linearForm x Pi
  case p of
    (Number n, b) ->
      let (m, r) = properFraction n
          q = cases r b
       in if even m
            then q
            else q >>= onOddPi
    _ -> fail "Could not handle period"

simplifyFun :: PExpr -> EvalResult PExpr
-- Funciones trigonometricas inversas
simplifyFun (Asin (Sin x)) = return x
simplifyFun (Acos (Cos x)) = return x
simplifyFun (Sin (Asin x)) = return x
simplifyFun (Cos (Acos x)) = return x
simplifyFun (Tan (Atan x)) = return x
simplifyFun (Atan (Tan x)) = return x

-- funciones trigonometricas
simplifyFun (Sin x)
  | mulByNeg x = simplifyNegate x >>= simplifyFun . Sin >>= simplifyNegate
simplifyFun (Sin x) = handlePeriod cases simplifyNegate x <|> return (Sin x)
  where
    cases r b = case (r, b) of
      (0, Number 0) -> return (Number 0)
      (0, _) ->
        if mulByNeg b
          then simplifyNegate b >>= simplifyNegate . Sin
          else return $ Sin b
      (_, Number 0) | r == 1 / 6 -> return $ Number $ 1 / 2
      (_, Number 0) | r == 1 / 4 -> simplifySqrt (Number 2) >>= (`simplifyDiv` Number 2)
      (_, Number 0) | r == 1 / 3 -> simplifySqrt (Number 3) >>= (`simplifyDiv` Number 2)
      (_, Number 0) | r == 1 / 2 -> return $ Number 1
      (_, _) -> Sin <$> (simplifyProduct [Number r, Pi] >>= simplifySum . (: [b]))
simplifyFun (Cos x)
  | mulByNeg x = simplifyNegate x >>= simplifyFun . Cos
simplifyFun (Cos x) = handlePeriod cases simplifyNegate x <|> return (Cos x)
  where
    cases r b = case (r, b) of
      (0, Number 0) -> return $ Number 1
      (0, _) ->
        if mulByNeg b
          then Cos <$> simplifyNegate b
          else return $ Cos b
      (_, Number 0) | r == 1 / 6 -> simplifySqrt (Number 3) >>= (`simplifyDiv` Number 2)
      (_, Number 0) | r == 1 / 4 -> simplifySqrt (Number 2) >>= (`simplifyDiv` Number 2)
      (_, Number 0) | r == 1 / 3 -> return $ Number $ 1 / 2
      (_, Number 0) | r == 1 / 2 -> return $ Number 0
      (_, _) -> Cos <$> (simplifyProduct [Number r, Pi] >>= simplifySum . (: [b]))

-- exponenciales
simplifyFun (Exp (Log x)) =
  if mulByNeg x
    then simplifyNegate x
    else return x
simplifyFun (Exp x) = do
  y' <- simplifyPow e x
  case y' of
    Pow (Exp (Number 1)) y -> return $ Exp y
    y -> return y
  where
    e = Exp (Number 1)

-- logaritmos
simplifyFun (Log (Number 1)) = return $ Number 0
simplifyFun (Log (Exp x)) = return x
simplifyFun (Log x)
  | true $ isNegative x = fail "Logaritmo de un número negativo"
  | otherwise = return $ Log x

-- funciones conocidas
simplifyFun (Fun "sqrt" [x]) = simplifySqrt x
simplifyFun (Fun "tan" [x]) = do
  s <- simplifyFun $ Sin x
  c <- simplifyFun $ Cos x
  simplifyDiv s c
simplifyFun (Fun "csc" [x]) = simplifyFun (Fun "sin" [x]) >>= simplifyDiv (Number 1)
simplifyFun (Fun "sec" [x]) = simplifyFun (Fun "cos" [x]) >>= simplifyDiv (Number 1)
simplifyFun (Fun "cot" [x]) = simplifyFun (Fun "tan" [x]) >>= simplifyDiv (Number 1)
simplifyFun (Fun "sinh" [x]) = do
  e <- simplifyFun $ Exp x
  e' <- simplifyNegate x >>= simplifyFun . Exp
  simplifySum [e, e'] >>= simplifyDiv (Number 2)
simplifyFun (Fun "cosh" [x]) = do
  e <- simplifyFun $ Exp x
  e' <- simplifyNegate x >>= simplifyFun . Exp
  simplifySum [e, e'] >>= simplifyDiv (Number 2)
simplifyFun (Fun "tanh" [x]) = do
  s <- simplifyFun $ Fun "sinh" [x]
  c <- simplifyFun $ Fun "cosh" [x]
  simplifyDiv s c
simplifyFun (Fun "csch" [x]) = simplifyFun (Fun "sinh" [x]) >>= simplifyDiv (Number 1)
simplifyFun (Fun "sech" [x]) = simplifyFun (Fun "cosh" [x]) >>= simplifyDiv (Number 1)
simplifyFun (Fun "coth" [x]) = simplifyFun (Fun "tanh" [x]) >>= simplifyDiv (Number 1)
simplifyFun x = return x

------------------
----