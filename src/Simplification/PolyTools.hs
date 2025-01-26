{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Simplification.PolyTools where
import Prelude hiding (exponent)

import PExpr

import Data.Bifunctor

import qualified Number as N

import Control.Monad (unless, foldM)
import qualified Simplification.Algebraic as Algebraic

import Simplification.Rationalize (rationalize)
import Data.List (union)
import Symplify


isSymbol :: PExpr -> Bool
isSymbol (Fun _ _) = True
isSymbol _ = False

variables :: PExpr -> [PExpr]
variables (Number _) = []
variables u@(Pow v w)
    | true (isInteger w &&& w > 1) = variables v
    | otherwise = [u]
variables (Add us) = foldl union [] $ map variables us
variables (Mul us) = foldl union [] $ map variables us
variables u = [u]

-- * Manipulación de polinomios

{-|
    Dada una expresion algebraica, si la expresion es un monomio sobre \(x\) entonces 
    devuelve el par \((c,m)\) donde \(c\) es el coeficiente del monomio y \(m\) es el grado del monomio.
    Si la expresion no es un monomio sobre \(x\) entonces devuelve un error.
-}
coefficientMonomialGPE :: PExpr -> PExpr -> EvalSteps (PExpr, Integer)
coefficientMonomialGPE u x
    | u == x = return (1, 1)
coefficientMonomialGPE (Pow base (Number exponent)) x
    | true $ (base == x) &&& isInteger exponent &&& (exponent > 1) = return (1, N.numerator exponent)
coefficientMonomialGPE u@(Mul us) x = mapM (`coefficientMonomialGPE` x) us >>= foldM combine (u,0)
    where
        combine (c,m) (_,0) = return (c,m)
        combine (_, _) (_, m) = simplifyPow x (fromInteger m) >>= simplifyDiv u >>= \c -> return (c, m)
coefficientMonomialGPE u x
    | freeOf u x = return (u,0)
    | otherwise = fail $ show u ++ " no es un monomio sobre " ++ show x


{-|
    Devuelve el grado de una expresion algebraica \(u\) respecto a la variable \(x\), siempre y cuando \(u\) sea un polinomio
    sobre \(x\). Si \(u\) no es un polinomio sobre \(x\) entonces devuelve un error.

    Ejemplos:

    > degreeGPE 0 x = -1
    > degreeGPE (x^2 + 2*x + 1) x = 2
    > degreeGPE (x**2 + 2*y*x) x = 2
    > degreeGPE (x**2 + 2*y*x) y = 1
    > degreeGPE (e^x) x = Undefined: e^x no es un monomio sobre x
-}
degreeGPE :: PExpr -> PExpr -> EvalSteps Integer
degreeGPE 0 _ = return (-1)
degreeGPE (Add us) x = foldM (\d u -> max d . snd <$> coefficientMonomialGPE u x) (-1) us --maximum $ map (`degreeGPE` x) us
degreeGPE u x = snd <$> coefficientMonomialGPE u x

{-|
    Devuelve el coeficiente del monomio \(x^j) de una expresion algebraica (\u\), siempre y cuando \(u\) sea un polinomio
    sobre \(x\). Si \(u\) no es un polinomio sobre \(x\) entonces devuelve un error.

    Ejemplos:

    > coefficientGPE 0 x 21 = 0 
    > coefficientGPE (x^2 + 2*x + 1) x 2 = 1
    > coefficientGPE (y*x**2 + 2*y*x) x 1 = 2*y
    > coefficientGPE (y*x**2 + 2*y*x) y 1 = x**2 + 2*x
    > coefficientGPE (e^x) x 2 = Undefined: e^x no es un monomio sobre x
-}
coefficientGPE :: PExpr -> PExpr -> Integer -> EvalSteps PExpr
coefficientGPE u@(Add us) x j
    | u == x = if j == 1 then return 1 else return 0
    | otherwise = foldM combine 0 us
        where
            combine c mon = do
                                (c', m) <- coefficientMonomialGPE mon x
                                if m == j
                                    then simplifySum [c,c']
                                    else return c
coefficientGPE u x j = do
                        (c,m) <- coefficientMonomialGPE u x
                        if j == m
                            then return c
                            else return 0

{-|
    Devuelve la lista de los coeficientes del polinomio \(u\) sobre \(x\). Si \(u\) no es un polinomio sobre \(x\)
    entonces devuelve un error.
-}
coefficientListGPE :: PExpr -> PExpr -> EvalSteps [PExpr]
coefficientListGPE u x = do
                            m <- degreeGPE u x
                            mapM (coefficientGPE u x) [0..m]

{-|
    Devuelve el coeficiente lider del polinomio \(u\) sobre \(x\). EL coeficiente líder es el coeficiente del monomio
    de mayor grado en \(u\). Si \(u\) no es un polinomio sobre \(x\) entonces devuelve un error.
-}
leadingCoefficient :: PExpr -> PExpr -> EvalSteps PExpr
leadingCoefficient 0 _ = fail "leadingCoefficient: 0 no tiene coeficiente líder"
leadingCoefficient u x = do
                            m <- degreeGPE u x
                            coefficientGPE u x m

{-|
    Devuelve el monomio líder del polinomio multivariable \(u\) sobre una lista de simbolos. Se define el monomio lider
    como aquel que es mayor en orden lexicografico.

    Un monomio \(u\) es menor lexicograficamente que un monomio \(v\) sobre una lista de simbolos \([x_1,x_2,\dots,x_n\)
    si se cumple al menos una de 2 condiciones:

        (1) \(\operatorname{deg}(u,x_1) < \operatorname{deg}(v,x_1)\)

        (2) Para algún \(1<j\leq n\), \(\operatorname{deg}(u,x_i) = \operatorname{deg}(v,x_i)\) para \(i=1,2,\dots,j-1\),
        y además \(\operatorname{deg}(u,x_j) < \operatorname{deg}(v,x_j)\)

    El monomio lider depende del orden de las variables en la lista de simbolos.

    > u = 3*x^2*y+4*x*y**2+y^3+x+1
    > leadingMonomial u [x,y] = 3*x^2*y
    > leadingMonomial u [y,x] = y^3

-}
leadingMonomial :: PExpr -> [PExpr] -> EvalSteps PExpr
leadingMonomial p symbols = do
                                unless (all isSymbol symbols) $ fail "leadingMonomial: not all arguments are symbols"
                                leadingMonomial' symbols p
    where
        leadingMonomial' _ 0 = fail "leadingCoefficient: 0 no tiene coeficiente líder"
        leadingMonomial' [] u = return u
        leadingMonomial' (x:l) u = do
                                    m <- degreeGPE u x
                                    c <- coefficientGPE u x m
                                    xm <- simplifyPow x (fromInteger m)
                                    lm <- leadingMonomial' l c
                                    simplifyProduct [xm,lm]

-- * Division de polinomios

{-|
    Algoritmo que permite obtener la division entre \(u\) y \(v\) donde ambos son polinomios
    multivariables en \(\mathbb{Q}[x_1,x_2,...,x_n]\), la lista de simbolos \(l=[x_1,x_2,...,x_n]\) se pasa
    como argumento junto a los polinomios a dividir. 

    Se dice que el algoritmo es recursivo porque la division en terminos de la variable principal \(x_1\) depende
    recursivamente de la division de polinomios en \(\mathbb{Q}[x_2,...,x_n]\).

    Devuelve el par \((q,r)\) donde \(q\) es el cociente y \(r\) es el resto de la division de \(u\) entre \(v\). Los
    cuales satisfacen:

    \[ u = q\cdot v + r \]
    \[ \operatorname{deg}(r,x) < \operatorname{deg}(v,x) \ \  \lor \ \ lc(v,x_1) \not| \ \  lc(r, x) \]
    \[ \text{Si } u|v \implies r = 0\]

    La propiedad \(\operatorname{deg}(r,x) < \operatorname{deg}(v,x)\) se denomina __propiedad euclidiana de la division polinomica__
    y es fundamental para el calculo del maximo comun divisor entre polinomios. La propiedad euclideana siempre se satisface si los
    coeficientes del polinomio forman un cuerpo. Si los coeficientes no forman un cuerpo, la propiedad euclideana puede o no cumplirse.
-}
recPolyDivide :: PExpr -> PExpr -> [PExpr] -> EvalSteps (PExpr, PExpr)
recPolyDivide u v [] = do
                        udivv <- simplifyDiv u v
                        return (udivv, 0)
recPolyDivide u v (x:tl) = do 
                            let r = u
                            m <- degreeGPE u x
                            n <- degreeGPE v x
                            let q = 0
                            lcv <- leadingCoefficient v x
                            recPolyDivideLoop lcv q r m n
    where
        recPolyDivideLoopReturn q r = Algebraic.expand' q >>= \q' -> return (q',r)

        recPolyDivideLoop lcv q r m n
            | m >= n = do
                        lcr <- leadingCoefficient r x
                        d <- recPolyDivide lcr lcv tl
                        if snd d /= 0
                            then recPolyDivideLoopReturn q r
                            else do
                                let c = fst d
                                xmn <- simplifyPow x (fromInteger $ m-n)
                                q <- simplifyProduct [c,xmn] >>= \t -> simplifySum [q,t] -- q' = q + c*x^(m-n)
                                r <- simplifyProduct [c, v, xmn]  >>= simplifySub r >>= Algebraic.expand' -- r' = r - c*v*x^(m-n)
                                m <- degreeGPE r x
                                recPolyDivideLoop lcv q r m n
            | otherwise = recPolyDivideLoopReturn q r


{-|
    Equivalente a 'recPolyDivide' pero devuelve solo el cociente de la division de \(u\) y \(v\).
-}
recQuotient :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
recQuotient u v l = fst <$> recPolyDivide u v l

{-|
    Equivalente a 'recPolyDivide' pero devuelve solo el resto de la division de \(u\) y \(v\).
-}
recRemainder :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
recRemainder u v l = snd <$> recPolyDivide u v l

{-|
    Algoritmo alternativo para dividir polinomios multivariables en un conjunto de simbolos. Se basa en la estructura monomial de los polinomios
    en lugar de su estructura recursiva. Puede determinar si \(u | v\), pero si \(u \! \! \! \not | v\) puede producir un resultado
    distinto a la division recursiva.

    Devuelve el par \((q,r)\) donde \(q\) es el cociente y \(r\) es el resto de la division de \(u\) entre \(v\). Los polinomios \(q\) y \(r\)
    satisfacen varias propiedades, entre ellas:

    1. \( u = q\cdot v + r \)
    2. \(lm(v,l)\) no divide a ningún monomio de \(r\)
    3. \(q\) y \(r\) son los únicos polinomios que satisfacen las propiedades anteriores, para el orden de las variables dado
    4. Si \(u|v \implies r = 0\)

    \(lm(v,l)\) es el monomio líder de \(v\) respecto a la lista de simbolos \(l\).
-}
mbPolyDivide :: PExpr -> PExpr -> [PExpr] -> EvalSteps (PExpr, PExpr)
mbPolyDivide u v l = do
                    let q = 0
                        r = u
                    vl <- leadingMonomial v l
                    f <- g r vl
                    polyDivide' q r f vl
    where
        polyDivide' q r f vl
            | f /= 0 = do
                        q <- simplifySum [q,f]
                        r <- Algebraic.expand $ simplifySub r =<< simplifyProduct [f,v]
                        f <- g r vl
                        polyDivide' q r f vl
            | otherwise = return (q,r)

        g (Add us) vm = mapM (`g` vm) us >>= simplifySum  -- g = sum ui / vm, donde ui es divisible por vm
        g w wm = do
                    w' <- simplifyDiv w wm
                    dw <- denominator w'
                    if dw == 1
                        then return w'
                        else return 0

-- | Equivalente a 'mbPolyDivide' pero devuelve solo el cociente de la division de \(u\) y \(v\).
mbQuotient :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
mbQuotient p q l = fst <$> mbPolyDivide p q l

-- | Equivalente a 'mbPolyDivide' pero devuelve solo el resto de la division de \(u\) y \(v\).
mbRemainder :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
mbRemainder p q l = snd <$> mbPolyDivide p q l

{-|
    Proceso similar a la división de polinomios donde el resto de la división satisface la propiedad euclidiana
    \[(q,r) = \operatorname{PseudoDivision}(u,v) \implies \operatorname{deg}(r) < \operatorname{deg}(v)\]
    
    Gracias a que se satisface esta propiedad, es factible usar este proceso de división para computar
    el máximo común divisor entre polinomios.

    Retorna dos polinomios \(q\) y \(r\) denominados __pseudo-cociente__ y __pseudo-resto__ respectivamente, los cuales
    satisfacen:
    
    \[ \operatorname{lc}(v,x)^\delta u = q\cdot v + r \]
    \[ \operatorname{deg}(r,x) < \operatorname{deg}(v,x) \]

    Donde:
    
        * \(\operatorname{lc}(v,x)\) es el coeficiente líder de \(v\) respecto a \(x\).

        * \(\delta\) es \(\max(\operatorname{deg}(u,x) - \operatorname{deg}(v,x) + 1, 0)\).

        * \(u\) y \(v\) son los polinomios a dividir.

        * \(x\) es la variable respecto a la cual se realiza la división.
    
    Ejemplos:

    > pseudoDivision (x^2 + 2*x + 1) (x + 1) x = (x + 1, 0) -- 1^(2) * (x^2 + 2*x + 1) = (x + 1)(x + 1) + 0
    > pseudoDivision (2*x+2*y) 2 x = (4*x+4*y, 0) -- 2^2 * (2*x+2*y) = (4*x+4*y)*2 + 0
    

-}
pseudoDivision :: PExpr -> PExpr -> PExpr -> EvalSteps (PExpr, PExpr)
pseudoDivision _ 0 _ = fail "Pseudo-division by zero"
pseudoDivision u v x = do
                        m <- degreeGPE u x
                        n <- degreeGPE v x
                        let delta = max (m-n+1) 0
                        lcv <- coefficientGPE v x n -- Equivalente a lcv = leadingCoefficient v x, pero mas eficiente porque el grado ya esta computado
                        let sigma = 0
                        pseudoDivision' 0 u m n delta lcv sigma
    where
        pseudoDivision' p s m n delta lcv sigma
            | m >= n = do
                        lcs <- coefficientGPE s x m -- Equivalente a lcs = leadingCoefficient s x, pero mas eficiente porque el grado ya esta computado
                        x' <- simplifyPow x (fromInteger $ m-n) -- 1
                        p <- do
                                a' <- simplifyProduct [lcv,p]
                                b' <- simplifyProduct [lcs,x']
                                simplifySum [a',b']
                        s <- do
                                a' <- simplifyProduct [lcv, s]
                                b' <- simplifyProduct [lcs, v, x']
                                Algebraic.expand $ simplifySub a' b'
                        -- let sigma = sigma + 1
                        m <- degreeGPE s x
                        pseudoDivision' p s m n delta lcv (sigma+1)
            | otherwise = do
                            lcv' <- simplifyPow lcv (fromInteger $ delta-sigma)
                            q <- Algebraic.expand $ simplifyProduct [lcv',p]
                            r <- Algebraic.expand $ simplifyProduct [lcv',s]
                            return (q,r)

-- | Equivalente a 'pseudoDivision' pero devuelve solo el pseudo-resto de la division de \(u\) y \(v\).
pseudoRem :: PExpr -> PExpr -> PExpr -> EvalSteps PExpr
pseudoRem u v x = snd <$> pseudoDivision u v x


-- * Maximo común divisor

{-|
    Normaliza un polinomio multivariable. Un polinomio multivariable sobre \(\mathbb{Q}[x_1,x_2,\dots,x_n]\) esta normalizado si,
    al verlo como un polinomio con coeficientes en \(\mathbb{Q}[x_2,\dots,x_n]\), el coeficiente líder esta normalizado.

    Un polinomio en \(\mathbb{Q}[x]\) esta normalizado si es 0 o si su coeficiente lider es 1.
-}
normalize :: Foldable t => PExpr -> t PExpr -> EvalSteps PExpr
normalize 0 _ = return 0
-- Dividir por el coeficiente lider entre todos los coeficientes y expandir
normalize u l = foldM leadingCoefficient u l  >>= simplifyDiv u >>= Algebraic.expand'

{-|
    Verifica si un polinomio multivariable esta normalizado
-}
normalized :: Foldable t => PExpr -> t PExpr -> EvalSteps Bool
normalized u l = do
                    lc <- foldM leadingCoefficient u l
                    return $ lc == 1 || lc == 0

{-| 
    El contenido de un polinomio en \(\mathbb{K}[x]\) se define como sigue:
        
        1. Si \(u\) es una suma de al menos 2 monomios distintos de cero, el contenido
        es el máximo común divisor de sus coeficientes distintos de cero(en \(\mathbb{K}\)).
    
        2. Si \(u\) es un monomio, el contenido es el resultado de normalizar el coeficiente
        del monomio.

        3. Si \(u=0\), el contenido es \(0\).

    Esta función calcula el contenido del polinomio \(u\) con variable principal \(x\) y usa la
    lista de variables auxiliares \(r\) para calcular el máximo común divisor de los coeficientes.
-}
polyContent :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyContent u x r = do
                      cfl <- coefficientListGPE u x 
                      gcdList cfl r

{-| 
    Se define la parte primitiva de un polinomio \(u\) en \(\mathbb{K}[x]\) como el resultado de dividir
    \(u\) por su contenido. Esta función calcula la parte primitiva de \(u\) con respecto a la variable principal
    \(x\) y la lista de variables auxiliares \(r\).	
-}
polyPrimitivePart :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyPrimitivePart 0 _ _ = return 0
polyPrimitivePart u x r = do
                            contU <- polyContent u x r
                            recQuotient u contU (x:r)

{-|
    Calculo del maximo común divisor en el dominio de polinomios multivariados \(\mathbb{Q}[x_1,x_2,\dots,x_n\),
    las variables \(x_1,x_2,\dots,x_n\) son pasadas como argumento en la lista @l@.

    El maximo común divisor entre 2 polinomios \(u\) y \(v\) en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\) se define como el polinomio
    \(d\) que cumple las siguientes propiedades:

        1. \(d\) es un divisor común de \(u\) y \(v\), es decir, \(d|u\) y \(d|v\).
        2. Si \(e\) es un divisor común de \(u\) y \(v\), entonces \(e|d\).
        3. \(d\) esta normalizado si es un polinomio o \(d=1\).
    
    Si \(u\) y \(v\) son polinomios nulos, la definición de arriba no aplica, en este caso el maximo común divisor es 0.
-}
polyGCD :: PExpr -> PExpr -> [PExpr] -> EvalSteps PExpr
polyGCD 0 v l = normalize v l
polyGCD u 0 l = normalize u l
polyGCD u v l = polyGCDRec u v l >>= (`normalize` l)
    where
        polyGCDRec _ _ [] = return 1 -- gcd(u,v) where u and v are non-zero rationals is 1
        polyGCDRec u v l@(x:rest) = do
                                    contU <- polyContent u x rest
                                    contV <- polyContent v x rest
                                    
                                    d <- polyGCDRec contU contV rest
                                    
                                    ppU <- recQuotient u contU l -- primitive part of u
                                    ppV <- recQuotient v contV l -- primitive part of v

                                    rp <- gcdLoop x rest ppU ppV

                                    Algebraic.expand $ simplifyProduct [d,rp]
        
        gcdLoop _ _ ppU 0 = return ppU
        gcdLoop x rest ppU ppV = do
                                    r <- pseudoRem ppU ppV x
                                    ppR <- polyPrimitivePart r x rest
                                    gcdLoop x rest ppV ppR 
{-|
    Computa la secuencia de restos primitivos, la cual se utiliza para computar el maximo común divisor entre polinomios
    multivariables.

    Si \(u\) y \(v\) son polinomios con variable principal \(x\), la secuencia de restos primitivos se define como sigue:
        \[R_0 = PolyPrimitivePart(u)\]
        \[R_1 = PolyPrimitivePart(v)\]
        \[R_{n+2} = PolyPrimitivePart(PseudoRemainder(R_n, R_{n+1}, x))\]
    
    Dado que la pseudo-división satisface la propiedad euclidiana, la secuencia eventualmente converge a 0.

-}
remainderSequence :: PExpr -> PExpr -> [PExpr] -> EvalSteps [PExpr]
remainderSequence _ _ [] = error "Remainder sequence undefined for empty lists"
remainderSequence u v (x:rest) = do
                                    ppU <- polyPrimitivePart u x rest
                                    ppV <- polyPrimitivePart v x rest
                                    remainderSequence' ppU ppV
    where
        remainderSequence' ppU 0 = return [ppU, 0]
        remainderSequence' ppU ppV = do
                                        r <- pseudoRem ppU ppV x
                                        ppR <- polyPrimitivePart r x rest
                                        rs <- remainderSequence' ppV ppR
                                        return $ ppU:rs

{-|
    Calcula el maximo común divisor entre una lista de polinomios multivariables en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\).
    La lista @l@ contiene las variables \(x_1,x_2,\dots,x_n\).
-}
gcdList :: [PExpr] -> [PExpr] -> EvalSteps PExpr
gcdList [] _ = return 0
gcdList [p] l = normalize p l 
gcdList (p:ps) r = do
                    ps' <- gcdList ps r
                    polyGCD p ps' r

{-|
    Calcula el minimo común multiplo entre una lista de polinomios multivariables en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\).
    La lista @l@ contiene las variables \(x_1,x_2,\dots,x_n\). El calculo se realiza aprovechando la siguiente propiedad:

    \[\operatorname{lcm}(a_1,a_2,\dots,a_n) = \dfrac{\prod_{i=1}^n a_i}{\gcd(b_1,b_2,\dots,b_n)}\]

    \[b_i = \prod_{j=1,j\neq i}^n a_j\]
-}
lcmList :: [PExpr] -> EvalSteps PExpr
lcmList us = do
                n <- Algebraic.expand $ simplifyProduct us
                let v = variables n
                let d = removeEachElement us
                d' <- mapM (Algebraic.expand . simplifyProduct) d >>= (`gcdList` v)
                recQuotient n d' v
            where
                removeEachElement :: [a] -> [[a]]
                removeEachElement xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

rationalSimplify :: EvalSteps PExpr -> EvalSteps PExpr
rationalSimplify = (=<<) rationalSimplify'
    where
        numberCoefficientList (Number p) = [p]
        numberCoefficientList (Mul []) = [0]
        numberCoefficientList (Mul (Number p:_)) = [p]
        numberCoefficientList (Add us) = concatMap numberCoefficientList us
        numberCoefficientList _ = [1]

        signNormalized p v = do
                                lcp <- foldM leadingCoefficient p v
                                return (lcp == 1 || lcp == 0)

        simplfyNumbers _ 0 = fail "Division by zero"
        simplfyNumbers 0 _ = return (0,1)
        simplfyNumbers n d = let
                                c = numberCoefficientList n ++ numberCoefficientList d
                                (n',d') = foldr (\x -> bimap (gcd (N.numerator x)) (lcm (N.denominator x))) (0,1) c  -- n' is lcm of the denominators and d' is the gcd of the numerators
                             in do
                                  c' <- simplifyDiv (fromInteger n') (fromInteger d')
                                  n'' <- simplifyDiv n c'
                                  d'' <- simplifyDiv d c'
                                  return (n'',d'')

        simplifySign n d v = do
                                normn <- signNormalized n v
                                normd <- signNormalized d v
                                n' <- if normn then return n else simplifyNegate n
                                d' <- if normd then return d else simplifyNegate d
                                if normn == normd -- if both n and d had the same sign, do nothing otherwise negate the quotient
                                    then simplifyDiv n' d'
                                    else simplifyDiv n' d' >>= simplifyNegate
        
        simplifyNumberAndSign n d v = do
                                        (n',d') <- simplfyNumbers n d
                                        simplifySign n' d' v
        rationalSimplify' u = do
                                u' <- rationalize u
                                n <- Algebraic.expand $ numerator u'
                                d <- Algebraic.expand $ denominator u'
                                let v = variables n `union` variables d
                                ggcd <- polyGCD n d v
                                n' <- recQuotient n ggcd v
                                d' <- recQuotient d ggcd v
                                simplifyNumberAndSign n' d' v