{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Calculus.Integrate
-- Description : Proporciona funcionalidad para calcular la integral de expresiones matemáticas.
--
-- Este módulo define funciones para calcular la integral de expresiones matemáticas con respecto a una variable dada. Soporta la integración de operaciones aritméticas básicas, funciones trigonométricas, funciones exponenciales y logarítmicas.
-- Puede manejar la integración de expresiones mas complejas gracias a la aplicación de la linealidad de la integral y el método de sustitución.

module Calculus.Integrate where

import Expr

import Control.Applicative

import qualified Simplification.Algebraic as Algebraic (expand)

import Data.Bifunctor
import Data.TwoList (toList)
import Calculus.Derivate (derivate)
import Data.List (union)

import Calculus.Utils

-- $setup
-- >>> let x = symbol "x"
-- >>> let y = symbol "y"
-- >>> let z = symbol "z"
-- >>> let u = symbol "u"
-- >>> let f = function "f"
-- >>> let g = function "g"

-- * Constructores
{-|
    Construye una integral sin evaluar

    >>> makeUnevaluatedIntegral u x
    Integral(u,x)
-}
makeUnevaluatedIntegral :: Expr -> Expr -> Expr
makeUnevaluatedIntegral u x = function "Integral" [u,x]

{-|
    Construye una integral definida sin evaluar

    >>> makeUnevaluatedDefiniteIntegral u x z u
    Definite_Integral(u,x,z,u)
-}
makeUnevaluatedDefiniteIntegral :: Expr -> Expr -> Expr -> Expr -> Expr
makeUnevaluatedDefiniteIntegral u x a b = function "Definite_Integral" [u,x,a,b]

-- * Calculo de integrales
{-| 
    @integralTable e x@ realiza la integración de expresiones conocidas respecto a la variable @x@
    , por ejemplo, como \(\sin(x), e^x, \sec(x)\tan(x)\), etc

    Devuelve la integral de la expresión \(e\) si la conoce o @Undefined@ en caso contrario

    @integralTable@ cumple el rol de una tabla de integrales.

    === Ejemplos:
    
    >>> integralTable (log x) x
    log(x)*x-x
    >>> integralTable (x**2) x
    x^3/3
    >>> integralTable (2 * sin x * cos x) x
    Undefined: Integral no aparece en la tabla de integrales
-}
integralTable :: Expr -> Expr -> Expr
integralTable e x
    | e `freeOf` x = e * x -- Expresiones libres de x(constantes)
    | e == x = 1/2 * x ** 2 -- Integral de x
-- x^n con n libre de x
-- b^x con b libre de x
integralTable (Pow a n) x
    | a == x && n `freeOf` x = case n of
                                 -1 -> log x
                                 _ -> x**(n+1) / (n+1)
    | n == x && a `freeOf` x = a**x / log a
-- Integracion de funciones conocidas
integralTable f@(Fun _ (a:|[])) x
    | a == x = integrateFun f
    where
        integrateFun (Exp x) = exp x
        integrateFun (Log x) = x * log x - x
        integrateFun (Sin x) = - cos x
        integrateFun (Cos x) = sin x
        integrateFun _ = undefinedExpr "Integral no aparece en la tabla de integrales"
integralTable _ _ = undefinedExpr "Integral no aparece en la tabla de integrales"

---

{-|
    @separateFactors u x@ factoriza la expresión en 2 partes \(a \cdot b\) donde \(a\) no depende de \(x\)

    === Ejemplos:

    >>> separateFactors (x**2) x
    (1,x^2)
    
    >>> separateFactors (x**2 + 1) x
    (1,x^2+1)
    
    >>> separateFactors (21*x) x
    (21,x)

    >>> separateFactors (2*x*y) x
    (2*y,x)

-}
separateFactors :: Expr -> Expr -> (Expr, Expr)
separateFactors (Mul us) x = bimap product product $ partitionMul $ toList us
    where
        partitionMul [] = ([],[])
        partitionMul (u:us)
            | u `freeOf` x = first (u:) $ partitionMul us
            | otherwise = second (u:) $ partitionMul us
separateFactors u x
    | u `freeOf` x = (u,1)
    | otherwise = (1,u)




{-|
    @linearProperties u x@ aplica la linealidad de la integral a la expresion \(u\) respecto a la variable \(x\)

    Si \(u = a \cdot b\) y \(a\) no depende de \(x\), entonces

    \[\int u \, dx = a \cdot \int b \, dx\]

    Si \(u = a + b\) entonces

    \[\int u \, dx = \int a \, dx + \int b \, dx\]

    Una vez aplicada la linealidad, la integral restante se evalua con la función 'integrate'

    Si no es posible aplicar linealidad, la función devuelve Undefined
-}
linearProperties :: Expr -> Expr -> Expr
linearProperties u@(Mul _) x = do
                                let (free,dependent) = separateFactors u x 
                                if free == 1
                                    then undefinedExpr "No se puede aplicar linealidad de la integral"
                                    else free * (integrate dependent x)
linearProperties u@(Add _) x = mapStructure (`integrate` x) u
linearProperties _ _ = undefinedExpr "No se puede aplicar linealidad de la integral"

{-|
    @substitutionMethod f x@ aplica el método de sustitución para resolver la integral de la expresión \(f\) respecto a la variable \(x\)

    Los posibles candidatos a sustitución son obtenidos con la función 'trialSubstituions'.
    
    Si se consigue una sustitución que eliina la variable @x@, entonces se evalua
    la integral de la expresión resultante con @x@

    Si no se consigue ninguna sustitución que elimine la variable x, la función devuelve Undefined.

    === Ejemplos
    
    >>> substitutionMethod (2*x*cos(x**2)) x
    sin(x^2)

    >>> substitutionMethod ((2*x+1)*cos(x**2+x)) x
    sin(x^2+x)

    >>> substitutionMethod (((x+1)*log(cos((x+1)**2))*sin((x+1)**2)) / (cos((x+1)**2))) x
    -log(cos((x+1)^2))^2/4
    
    >>> substitutionMethod (2*x/(x**4+1)) x 
    Undefined: No se puede aplicar sustitución

    El ultimo ejemplo evalua a \(\arctan(x^2)\) con la sustitución \(x^2\), pero como
    \(x^2\) no es obtenido por 'trialSubstituions', la integral no se puede obtener.
-}
substitutionMethod :: Expr -> Expr -> Expr
substitutionMethod f x = foldr ((<|>) . makeSubstitution) failSubstitution $ trialSubstituions f --do
                         --   let p = trialSubstituions f
                         --   foldr ((<|>) . makeSubstitution) failSubstitution p
    where
        failSubstitution = undefinedExpr "No se puede aplicar sustitución"

        -- Obtiene un nombre de variable de integración que no este en la expresion
        getIntegrationVariable u (Symbol x) = getIntegrationVariable' x
            where
                getIntegrationVariable' x = let
                                                _x = '_' : x
                                                symbol_x  = symbol $ _x
                                            in if u `freeOf` symbol_x
                                                then symbol_x
                                                else getIntegrationVariable' _x
        getIntegrationVariable _ _ = undefinedExpr "La variable de integración debe ser un simbolo"

        --makeSubstitution = undefined
        makeSubstitution g = if g/=x && not (g `freeOf` x)
                                then let
                                        f' = f / (derivate g x)
                                        v = getIntegrationVariable f' x
                                        u = substitute f' g v
                                     in if u `freeOf` x
                                        then substitute (integrate u v) v g
                                        else failSubstitution
                                else failSubstitution

---

{-|
    Similar a 'concatMap' pero realiza la unión en lugar de la concatenación.

    La unión elimina los elementos duplicados de la concatenación de listas.
-}
unionMap :: (Foldable t, Eq b) => (a -> [b]) -> t a -> [b]
unionMap f = foldl (\a b -> a `union` f b) []

{-|
    Obtiene posibles sustituciones a realizar por el metodo de sustitución.
    
    Hay 4 categorias de candidatos para sustitucion:

        1. Funciones
        2. Argumentos de funciones
        3. Bases de potencias
        4. Exponentes de potencias

    Por ejemplo, para la integral 

    \[\int \dfrac{(x+1)\ln(\cos((x+1)^2))\sin((x+1)^2))}{\cos((x+1)^2)} \, dx\]

        1. Funciones: \(\ln(\cos((x+1)^2))\), \(\cos((x+1)^2)\), \(\sin((x+1)^2))\)
        2. Argumentos de funciones: \(\cos((x+1)^2)\), \((x+1)^2\)
        3. Bases de potencias: \((x+1)\)
        4. Exponentes de potencias: \(2\)
-}
trialSubstituions :: Expr -> [Expr]
trialSubstituions u@(Fun _ (x:| xs)) = u : ((x:xs) `union` unionMap trialSubstituions (x:xs))
trialSubstituions (Pow a b) = [a,b] `union` trialSubstituions a `union` trialSubstituions b
trialSubstituions (Mul us) = unionMap trialSubstituions us
trialSubstituions (Add us) = unionMap trialSubstituions us
trialSubstituions _ = []


{-|
    @integrate u x@ evalua la integral de la expresión @u@ respecto a @x@.

    Realiza los siguientes pasos:

        1. Intenta evaluar la expresión usando la función @integralTable@
        2. Si esto falla, intenta aplica la linealidad de la integral, usando la función @linearProperties@
        3. Si esto falla, intenta aplicar el metodo de sustitución usando la función @substitutionMethod@

    Si todos los pasos fallan, no es posible evaluar la integral y la función devuelve Undefined
-}
integrate :: Expr -> Expr -> Expr
integrate f x = integralTable f x
                    <|>
                linearProperties f x
                    <|>
                substitutionMethod f x
                    <|>
                let g = Algebraic.expand f
                in if f /= g
                    then integrate g x -- Intentar integrar la expresion expandida
                    else makeUnevaluatedIntegral f x -- Integral desconocida, devolver una integral sin evaluar
---

{-|
    Calcula la integral definida de una expresion usando la regla de barrow

    1. Obtiene la integral de la expresión @u@ usando 'integrate'
    2. Crea dos expresiones @ub@ y @ua@ sustituyendo @x@ por @b@ y @a@ respectivamente
    3. Evalua la diferencia @ub - ua@
-}
definiteIntegral :: Expr -> Expr -> Expr -> Expr -> Expr
definiteIntegral u x a b = let
                            u' = integrate u x
                           in case u' of
                                Integral _ _ -> makeUnevaluatedDefiniteIntegral u x a b
                                _ -> (substitute u' x b) - (substitute u' x a)
