{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Calculus.Integrate where
import Symplify
import Data.List
import Data.Bifunctor

import Control.Applicative
import Calculus.Derivate (derivate)
import Simplification.PolyTools (variables)

{-|
    Sustitucion de expresiones dentro de otra expresion
-}
substitute :: PExpr -> PExpr -> PExpr -> EvalSteps PExpr
substitute u t r
    | u == t = return r
substitute u@(Number _) _ _ = return u
substitute (Add us) t r = mapM (\u -> substitute u t r) us >>= simplifySum
substitute (Mul us) t r = mapM (\u -> substitute u t r) us >>= simplifyProduct
substitute (Pow u v) t r = do
    u' <- substitute u t r
    v' <- substitute v t r
    simplifyPow u' v'
substitute (Fun f us) t r = mapM (\u -> substitute u t r) us >>= simplifyFun . Fun f

{-| 
    @integralTable e x@ realiza la integración de expresiones conocidas respecto a la variable @x@
    , por ejemplo, como \(\sin(x), e^x, \sec(x)\tan(x)\), etc

    Devuelve la integral de la expresión \(e\) si la conoce o @Undefined@ en caso contrario

    @integralTable@ cumple el rol de una tabla de integrales.

    Ejemplos:
    
    > integralTable (sec(x) * tan(x)) (x) = sec(x)
    > integralTable (log x) = log x * x
    > integralTable (x^2) = 1/3 * x^3
    > integralTable (2 * sin x * cos x) x = Undefined: Integral desconocida
-}
integralTable :: PExpr -> PExpr -> EvalSteps PExpr
-- Expresiones libres de x(constantes)
integralTable e x
    | e `freeOf` x = simplifyProduct [e,x]
    | e == x = simplifyPow x 2 >>= \x' -> simplifyProduct [1/2, x']
-- x^n con n libre de x
integralTable (Pow a n) x
    | a == x && n `freeOf` x = case n of
                                 -1 -> simplifyFun $ Log x
                                 _ -> do
                                    do 
                                        n' <- simplifySum [n,1] 
                                        x' <- simplifyPow x n'
                                        simplifyDiv x' n'
-- e^x, log x, b^x con b libre de x
integralTable (Exp a) x
    | a == x = return $ Exp a
integralTable (Log a) x
    | a == x = simplifyProduct [Log x, x]
integralTable (Pow b a) x
    | a == x && b `freeOf` x = do
                                x' <- simplifySum [x,1]
                                a' <- simplifyPow a x'
                                simplifyDiv a' x'
-- Funciones trigonometricas
integralTable f@(Fun _ [a]) x
    | a == x = integrateFun f
    where
        integrateFun (Sin x) = (simplifyFun . Cos) x >>= simplifyNegate
        integrateFun (Cos x) = simplifyFun $ Sin x
        integrateFun (Tan x) = simplifyFun (Cos x) >>= simplifyFun . Log >>= simplifyNegate
        integrateFun (Sec x) = do
                                tx <- simplifyFun $ Tan x
                                sx <- simplifyFun $ Sec x
                                simplifySum [tx,sx] >>= simplifyFun . Log
        integrateFun (Csc x) = simplifyDiv x 2 >>= simplifyFun . Tan >>= simplifyFun . Log
        integrateFun (Cot x) = simplifyFun (Sin x) >>= simplifyFun . Log
        integrateFun _ = fail "Integral desconocida"
-- Expresiones mas complicadas
-- integralTable (SecTan a) x = return $ Sec x
integralTable (Mul [Sec a, Tan b]) x
    | a==x && b==x = return $ Sec x
integralTable (Mul [Tan a, Sec b]) x
    | a==x && b==x = return $ Sec x
integralTable (Mul [Csc a, Cot b]) x
    | a==x && b==x = return $ Csc x
integralTable (Mul [Cot a, Csc b]) x
    | a==x && b==x = return $ Csc x
integralTable _ _ = fail "Integral desconocida"

---

{-|
    @separateFactors u x@ factoriza la expresión en 2 partes \(a \cdot b\) donde \(a\) no depende de \(x\)

    Ejemplos:

    > separateFactors (x^2) x = (1,x^2)
    > separateFactors (x^2 + 1) x = (1,x^2 + 1)
    > separateFactors 21 x = (21,1)
    > separateFactors (2*x*y) x = (2*y,x)
-}
separateFactors :: PExpr -> PExpr -> (PExpr, PExpr)
separateFactors (Mul us) x = bimap mkMul mkMul $ partitionMul us
    where
        mkMul [] = 1
        mkMul [u] = u
        mkMul us = Mul us

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
linearProperties :: PExpr -> PExpr -> EvalSteps PExpr
linearProperties u@(Mul _) x = do
                                let (free,dependent) = separateFactors u x 
                                if free == 1
                                    then fail "No se puede aplicar linealidad de la integral"
                                    else do
                                            i <- integrate dependent x
                                            simplifyProduct [free,i]
linearProperties (Add us) x = mapM (`integrate` x) us >>= simplifySum
linearProperties _ _ = fail "No se puede aplicar linealidad de la integral"

{-|
    @substitutionMethod f x@ aplica el método de sustitución para resolver la integral de la expresión \(f\) respecto a la variable \(x\)

    Los posibles candidatos a sustitución son obtenidos con la función 'trialSubstituions'.
    
    Si se consigue una sustitución que eliina la variable @x@, entonces se evalua
    la integral de la expresión resultante con @x@

    Si no se consigue ninguna sustitución que elimine la variable x, la función devuelve Undefined.
-}
substitutionMethod :: PExpr -> PExpr -> EvalSteps PExpr
substitutionMethod f x = do
                            let p = trialSubstituions f
                            foldr ((<|>) . makeSubstitution) failSubstitution p
    where
        failSubstitution = fail "No se puede aplicar sustitución"

        -- Obtiene un nombre de variable de integración que no este en la expresion
        getIntegrationVariable u (Symbol x) = getIntegrationVariable' x
            where
                vars = variables u
                getIntegrationVariable' x = let
                                                _x = '_' : x
                                            in if Symbol _x `elem` vars
                                                then getIntegrationVariable' _x
                                                else Symbol _x
        getIntegrationVariable _ _ = error "La variable de integración debe ser un simbolo"

        makeSubstitution g = if g/=x && not (g `freeOf` x)
                                then do
                                    f' <- derivate g x >>= simplifyDiv f -- f' = f / (dg/dx)
                                    let v = getIntegrationVariable f' x
                                    u <- substitute f' g v 
                                    if u `freeOf` x
                                        then do
                                                i <- integrate u v
                                                substitute i v g
                                        else failSubstitution
                                    
                                else failSubstitution

{-|
    @integrate u x@ evalua la integral de la expresión @u@ respecto a @x@.

    Realiza los siguientes pasos:

        1. Intenta evaluar la expresión usando la función @integralTable@
        2. Si esto falla, intenta aplica la linealidad de la integral, usando la función @linearProperties@
        3. Si esto falla, intenta aplicar el metodo de sustitución usando la función @substitutionMethod@

    Si todos los pasos fallan, no es posible evaluar la integral y la función devuelve Undefined
-}
integrate :: PExpr -> PExpr -> EvalSteps PExpr
integrate u x = integralTable u x
                    <|>
                linearProperties u x
                    <|>
                substitutionMethod u x
                    <|>
                fail "No se puede integrar la expresión"

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
trialSubstituions :: PExpr -> [PExpr]
trialSubstituions u@(Fun _ xs) = u : (xs `union` unionMap trialSubstituions xs)
trialSubstituions (Pow a b) = [a,b] `union` trialSubstituions a `union` trialSubstituions b
trialSubstituions (Mul us) = unionMap trialSubstituions us
trialSubstituions (Add us) = unionMap trialSubstituions us
trialSubstituions _ = []

---

{-|
    Calcula la integral definida de una expresion usando la regla de barrow

    1. Obtiene la integral de la expresión @u@ usando 'integrate'
    2. Crea dos expresiones @ub@ y @ua@ sustituyendo @x@ por @b@ y @a@ respectivamente
    3. Evalua la diferencia @ub - ua@
-}
definiteIntegral :: PExpr -> PExpr -> PExpr -> PExpr -> EvalSteps PExpr
definiteIntegral u x a b = do
                            u' <- integrate u x
                            ub <- substitute u' x b
                            ua <- substitute u' x a
                            simplifySub ub ua