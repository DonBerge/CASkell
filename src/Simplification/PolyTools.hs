{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Simplification.PolyTools where
import Prelude hiding (exponent)

import Expr
import Structure
import qualified Simplification.Algebraic as Algebraic

import Data.List

isSymbol :: Expr -> Bool
isSymbol (structure -> Symbol _) = True
isSymbol _ = False

variables :: Expr -> [Expr]
variables (structure -> Number _) = []
variables (structure -> MonomialTerm v _) = variables v
variables (structure -> Add us) = foldl union [] $ fmap variables us
variables (structure -> Mul us) = foldl union [] $ fmap variables us
variables u = [u]

-- * Manipulación de polinomios

{-|
    Dada una expresion algebraica, si la expresion es un monomio sobre \(x\) entonces 
    devuelve el par \((c,m)\) donde \(c\) es el coeficiente del monomio y \(m\) es el grado del monomio.
    Si la expresion no es un monomio sobre \(x\), \(c\) es Undefined y \(m\) es 0.
-}
coefficientMonomialGPE :: Expr -> Expr-> (Expr, Integer)
coefficientMonomialGPE 0 _ = (0,-1)
coefficientMonomialGPE u x
    | u == x = (1, 1)
coefficientMonomialGPE (structure -> MonomialTerm base exponent) x
    | base == x = (1, exponent)
coefficientMonomialGPE u@(structure -> Mul us) x = foldl combine (u,0) $ fmap (`coefficientMonomialGPE` x) us
    where
        combine (c,m) (_,0) = (c,m)
        combine (_,_) (_,m) = (u / (x ** (fromInteger m)),m)
coefficientMonomialGPE u _ = (u,0) -- TODO: NO ESTOY SEGURO DE ESTO

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
degreeGPE :: Expr -> Expr -> Integer
degreeGPE (structure -> Add us) x = maximum $ fmap (`degreeGPE` x) us -- foldM (\d u -> max d . snd <$> coefficientMonomialGPE u x) (-1) us --maximum $ map (`degreeGPE` x) us
degreeGPE u x = snd $ coefficientMonomialGPE u x

multidegree :: [Expr] -> Expr -> [Integer]
multidegree vars u = map (degreeGPE u) vars

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
coefficientGPE :: Expr -> Expr -> Integer -> Expr
coefficientGPE u@(structure -> Add us) x j
    | u == x = if j == 1 then 1 else 0
    | otherwise = foldl combine 0 us
        where
            combine c mon = let
                                (c', m) = coefficientMonomialGPE mon x
                            in if m==j
                                then c + c'
                                else c
coefficientGPE u x j = let
                        (c,m) = coefficientMonomialGPE u x
                       in
                        if j == m
                            then c
                            else 0

{-|
    Devuelve la lista de los coeficientes del polinomio \(u\) sobre \(x\). Si \(u\) no es un polinomio sobre \(x\)
    entonces devuelve un error.
-}
coefficientListGPE :: Expr -> Expr -> [Expr]
coefficientListGPE u x = let
                            m = degreeGPE u x
                         in
                            fmap (coefficientGPE u x) [0..m]

{-|
    Devuelve el coeficiente lider del polinomio \(u\) sobre \(x\). EL coeficiente líder es el coeficiente del monomio
    de mayor grado en \(u\). Si \(u\) no es un polinomio sobre \(x\) entonces devuelve un error.
-}
leadingCoefficient :: Expr -> Expr -> Expr
leadingCoefficient 0 _ = 0
leadingCoefficient u x = let
                            m = degreeGPE u x
                         in
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
leadingMonomial :: Expr -> [Expr] -> Expr
leadingMonomial 0 _ = 0
leadingMonomial u [] = u
leadingMonomial u (x:l) = let
                            m = degreeGPE u x
                            c = coefficientGPE u x m
                            xm = x ** (fromInteger m)
                            lm = leadingMonomial c l
                          in
                              lm * xm

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
recPolyDivide :: Expr -> Expr -> [Expr] -> (Expr, Expr)
recPolyDivide u v [] = (u/v, 0)
recPolyDivide u v (x:tl) = let
                            r = u
                            m = degreeGPE u x
                            n = degreeGPE v x
                            q = 0
                            lcv = leadingCoefficient v x
                           in
                            recPolyDivideLoop lcv q r m n
    where
        recPolyDivideLoopReturn q r = (Algebraic.expand q,r)

        recPolyDivideLoop lcv q r m n
            | m >= n = let
                        lcr = leadingCoefficient r x
                        d = recPolyDivide lcr lcv tl
                       in
                        if snd d /= 0
                            then recPolyDivideLoopReturn q r
                            else let
                                    c = fst d
                                    xmn = x ** (fromInteger $ m-n)
                                    q' = q + c*xmn
                                    r' = Algebraic.expand $ r - c*v*xmn
                                    m' = degreeGPE r' x
                                  in
                                    recPolyDivideLoop lcv q' r' m' n
            | otherwise = recPolyDivideLoopReturn q r


{-|
    Equivalente a 'recPolyDivide' pero devuelve solo el cociente de la division de \(u\) y \(v\).
-}
recQuotient :: Expr -> Expr -> [Expr] -> Expr
recQuotient u v l = fst $ recPolyDivide u v l

{-|
    Equivalente a 'recPolyDivide' pero devuelve solo el resto de la division de \(u\) y \(v\).
-}
recRemainder :: Expr -> Expr -> [Expr] -> Expr
recRemainder u v l = snd $ recPolyDivide u v l

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
mbPolyDivide :: Expr -> Expr -> [Expr] -> (Expr, Expr)
mbPolyDivide u v l = let
                        q = 0
                        r = u
                        vl = leadingMonomial v l
                        f = g r vl
                      in
                        polyDivide' q r f vl
    where
        polyDivide' q r f vl
            | f /= 0 =  let
                            q' = q+f
                            r' = Algebraic.expand $ r - f*v
                            f' = g r' vl
                        in
                            polyDivide' q' r' f' vl
            | otherwise = (q,r)


        g (structure -> Add us) vm = sum $ fmap (`g` vm) us --mapM (`g` vm) us >>= simplifySum  -- g = sum ui / vm, donde ui es divisible por vm
        g w wm = let
                    w' = w / wm
                    dw = denominator w'
                 in if dw == 1
                      then w'
                      else 0

-- | Equivalente a 'mbPolyDivide' pero devuelve solo el cociente de la division de \(u\) y \(v\).
mbQuotient :: Expr -> Expr -> [Expr] -> Expr
mbQuotient p q l = fst $ mbPolyDivide p q l

-- | Equivalente a 'mbPolyDivide' pero devuelve solo el resto de la division de \(u\) y \(v\).
mbRemainder :: Expr -> Expr -> [Expr] -> Expr
mbRemainder p q l = snd $ mbPolyDivide p q l


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
pseudoDivision :: Expr -> Expr -> Expr -> (Expr, Expr)
pseudoDivision _ 0 _ = let f = undefinedExpr "Pseudo-division por cero" in (f,f)
pseudoDivision u v x = let
                        p = 0
                        s = u
                        m = degreeGPE u x
                        n = degreeGPE v x
                        delta = max (m-n+1) 0
                        lcv = coefficientGPE v x n -- Equivalente a lcv = leadingCoefficient v x, pero mas eficiente porque el grado ya esta computado
                        sigma = 0
                       in 
                        pseudoDivision' p s m n delta lcv sigma
    where
        pseudoDivision' p s m n delta lcv sigma
            | m >= n = let
                        lcs = coefficientGPE s x m -- Equivalente a lcs = leadingCoefficient s x, pero mas eficiente porque el grado ya esta computado
                        x' = x ** (fromInteger (m-n))
                        p' = lcv * p + lcs * x' 
                        s' = Algebraic.expand $ lcv * s - lcs * v * x'
                        sigma' = sigma+1
                        m' = degreeGPE s' x
                        in
                            pseudoDivision' p' s' m' n delta lcv sigma'
            | otherwise = let
                            lcv' = lcv ** (fromInteger (delta-sigma))-- simplifyPow lcv (fromInteger $ delta-sigma)
                            q = Algebraic.expand $ lcv'*p
                            r = Algebraic.expand $ lcv'*s
                          in
                            (q,r)


-- | Equivalente a 'pseudoDivision' pero devuelve solo el pseudo-resto de la division de \(u\) y \(v\).
pseudoRem :: Expr -> Expr -> Expr -> Expr
pseudoRem u v x = snd $ pseudoDivision u v x

-- * Maximo común divisor

{-|
    Obtiene el coeficiente lider entre todos los coeficientes
-}
mostLeadingCoefficient :: Foldable t => Expr -> t Expr -> Expr
mostLeadingCoefficient = foldl leadingCoefficient

{-|
    Normaliza un polinomio multivariable. Un polinomio multivariable sobre \(\mathbb{Q}[x_1,x_2,\dots,x_n]\) esta normalizado si,
    al verlo como un polinomio con coeficientes en \(\mathbb{Q}[x_2,\dots,x_n]\), el coeficiente líder esta normalizado.

    Un polinomio en \(\mathbb{Q}[x]\) esta normalizado si es 0 o si su coeficiente lider es 1.
-}
normalize :: Foldable t => Expr -> t Expr -> Expr
normalize 0 _ = return 0
-- Dividir por el coeficiente lider entre todos los coeficientes y expandir
normalize u l = Algebraic.expand $ u / (mostLeadingCoefficient u l)

{-|
    Verifica si un polinomio multivariable esta normalizado
-}
normalized :: Foldable t => Expr -> t Expr -> Bool
normalized u l = let
                    lc = mostLeadingCoefficient u l
                 in
                    lc == 1 || lc == 0


{-| 
    El contenido de un polinomio \(u \in \mathbb{K}[x]\) se define como sigue:
        
        1. Si \(u\) es una suma de al menos 2 monomios distintos de cero, el contenido
        es el máximo común divisor de sus coeficientes distintos de cero(en \(\mathbb{K}\)).
    
        2. Si \(u\) es un monomio no nulo, el contenido es el resultado de normalizar el coeficiente
        del monomio.

        3. Si \(u=0\), el contenido es \(0\).

    Notar que si \(u \neq 0 \implies \operatorname{cont}(u,x) | u \)

    Esta función calcula el contenido del polinomio \(u\) con variable principal \(x\) y usa la
    lista de variables auxiliares \(r\) para calcular el máximo común divisor de los coeficientes.
-}
polyContent :: Expr -> Expr -> [Expr] -> Expr
polyContent u x r = let
                      cfl = coefficientListGPE u x 
                    in
                        gcdList cfl r



{-| 
    Se define la parte primitiva de un polinomio \(u\) en \(\mathbb{K}[x]\) como el resultado de dividir
    \(u\) por su contenido(Si \(u=0\), la parte primitiva es \(0\)). Esta función calcula la parte primitiva de \(u\) con respecto a la variable principal
    \(x\) y la lista de variables auxiliares \(r\).	
-}
polyPrimitivePart :: Expr -> Expr -> [Expr] -> Expr
polyPrimitivePart 0 _ _ = 0
polyPrimitivePart u x r = let
                            contU = polyContent u x r
                          in
                            recQuotient u contU (x:r)


{-|
    Calculo del maximo común divisor en el dominio de polinomios multivariados \(\mathbb{Q}[x_1,x_2,\dots,x_n]\),
    las variables \(x_1,x_2,\dots,x_n\) son pasadas como argumento en la lista @l@.

    El maximo común divisor entre 2 polinomios \(u\) y \(v\) en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\) se define como el polinomio
    \(d\) que cumple las siguientes propiedades:

        1. \(d\) es un divisor común de \(u\) y \(v\), es decir, \(d|u\) y \(d|v\).
        2. Si \(e\) es un divisor común de \(u\) y \(v\), entonces \(e|d\).
        3. \(d\) esta normalizado.
    
    Si \(u\) y \(v\) son polinomios nulos, la definición de arriba no aplica, en este caso el maximo común divisor es 0.
-}
polyGCD :: Expr -> Expr -> [Expr] -> Expr
polyGCD 0 v l = normalize v l
polyGCD u 0 l = normalize u l
polyGCD u v l = normalize (polyGCDRec u v l) l
    where
        polyGCDRec _ _ [] = 1 -- gcd(u,v) where u and v are non-zero rationals is 1
        polyGCDRec u v l@(x:rest) = let
                                    contU = polyContent u x rest
                                    contV = polyContent v x rest
                                    
                                    d = polyGCDRec contU contV rest
                                    
                                    ppU = recQuotient u contU l -- primitive part of u
                                    ppV = recQuotient v contV l -- primitive part of v

                                    rp = gcdLoop x rest ppU ppV

                                    in
                                        Algebraic.expand $ d*rp
        
        -- Computar los restos primitivos y obtener el penultimo resto
        gcdLoop _ _ ppU 0 = ppU
        gcdLoop x rest ppU ppV = let
                                    r = pseudoRem ppU ppV x
                                    ppR = polyPrimitivePart r x rest
                                 in
                                    gcdLoop x rest ppV ppR 

{-|
    Computa la secuencia de restos primitivos, la cual se utiliza para computar el maximo común divisor entre polinomios
    multivariables.

    Si \(u\) y \(v\) son polinomios con variable principal \(x\), la secuencia de restos primitivos se define como sigue:
        \[R_0 = PolyPrimitivePart(u)\]
        \[R_1 = PolyPrimitivePart(v)\]
        \[R_{n+2} = PolyPrimitivePart(PseudoRemainder(R_n, R_{n+1}, x))\]
    
    Dado que la pseudo-división satisface la propiedad euclidiana, la secuencia eventualmente converge a \(0\).

-}
remainderSequence :: Expr -> Expr -> [Expr] -> [Expr]
remainderSequence _ _ [] = []
remainderSequence u v (x:rest) = let
                                    ppU = polyPrimitivePart u x rest
                                    ppV = polyPrimitivePart v x rest
                                 in
                                    remainderSequence' ppU ppV
    where
        remainderSequence' ppU 0 = [ppU, 0]
        remainderSequence' ppU ppV = let
                                        r = pseudoRem ppU ppV x
                                        ppR = polyPrimitivePart r x rest 
                                     in
                                        ppU : (remainderSequence' ppV ppR)

{-|
    Calcula el maximo común divisor entre una lista de polinomios multivariables en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\).
    La lista @l@ contiene las variables \(x_1,x_2,\dots,x_n\).
-}
gcdList :: [Expr] -> [Expr] -> Expr
gcdList [] _ = 0
gcdList [p] l = normalize p l 
gcdList (p:ps) r = let
                    ps' = gcdList ps r
                   in
                    polyGCD p ps' r


{-|
    Calcula el minimo común multiplo entre una lista de polinomios multivariables en \(\mathbb{Q}[x_1,x_2,\dots,x_n]\).
    La lista @l@ contiene las variables \(x_1,x_2,\dots,x_n\). 
    
    El calculo se realiza aprovechando la siguiente propiedad:

    \[\operatorname{lcm}(a_1,a_2,\dots,a_n) = \dfrac{\prod_{i=1}^n a_i}{\gcd(b_1,b_2,\dots,b_n)}\]

    \[b_i = \prod_{j=1,j\neq i}^n a_j\]
-}
lcmList :: [Expr] -> Expr
lcmList us = let
                n = Algebraic.expand $ product us
                v = variables n
                d = removeEachElement us
                d' = (fmap (Algebraic.expand . product) d) `gcdList` v
             in
                recQuotient n d' v
            where
                removeEachElement :: [a] -> [[a]]
                removeEachElement xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

{-
-}