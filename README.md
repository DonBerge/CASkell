# CASkell: EDSL para el manejo simbolico de expresiones matematicas

## 1. Instalación del proyecto

## 2. Manejo basico de expresiones

Todo uso del EDSL necesita del tipo `Expr`, el cual se importa con la libreria homonima

```haskell
import Expr
```

### 2.1 Crear expresiones matemáticas
Todas las `Expr` se construyen a partir de 2 elementos base, numeros y simbolos:

#### Numeros
Los numeros se pueden crear a partir de la función 'fromNumber' o haciendo un casting explicito al tipo `Expr`:

```haskell
entero_dos = fromNumber 2
entero_tres = (3 :: Expr)
fraccion = fromNumber (21/19) -- fromNumber tiene mayor precedencia que '/' o cualquier operador matematico
```
Tambien hay soporte para numeros reales, pero seran tratados como fracciones si la misma no es muy grande. Los numeros reales tienen una presición fija y son sensibles a problemas de precisión
```haskell
(0.33 :: Expr) -- 33/10, fracción pequeña
(0.3333333 :: Expr) -- la fracción 3333333/10000000 es muy grande
(0.11111222223333344444 :: Expr) -- 0.11111222223333345, numero redondeado
```

El casting es necesario debido a que Haskell convierte los numeros a `Integer` o `Double` de manera predeterminada en vez de al tipo `Expr`. Aunque a veces no es necesario si ya se esta operando con `Expr`:

```haskell
x = symbol "x" -- x:: Expr
u = x+2 -- El casting no es necesario
```

En general, si la expresión a utiliza solo numeros o funciones de numeros, entonces es necesario hacer un casting.

#### Simbolos

##### Creación

Los símbolos se pueden crear utilizando la función `symbol`, que toma una cadena de texto como argumento y devuelve una expresión simbólica, el resultado de `symbol` puede asignarse a un identificador y ser combinado con otras expresiones.

```haskell
x = symbol "x"
y = symbol "y"
x+x+y -- 2*x+y
```
##### Suposiciones

Se pueden realizar suposiciones sobre los simbolos(ejemplo, es positivo o es entero) usando la función `assume`

```haskell
x = assume (symbol "x") ["even"]
y = assume (symbol "y") ["positive"]
n = assume (symbol "n") ["negative", "integer"]
```

Ciertas suposiciones pueden hacer que se ejecuten o no se ejecuten ciertas simplificaciones:
```haskell
(2*x)**n = 2**n * x**n -- Distribución de potencias con exponentes enteros
(2*x)**y = (2*x)**y -- No hay distribución ya que 'y' no es entero

0 ** y = 0 -- 0^x = 0 para y > 0
0 ** n = Undefined: division por cero -- n < 0 
0 ** x = 0^x -- No se sabe el signo de x, no se modifica la expresión
```

Las suposiciones no son retroactivas:
```haskell
u = 0**y -- 0
y = assume (symbol "y") ["negative"] -- y ahora es negativo
0**y -- Undefined: division por cero
v = u -- v = 0
```

##### Preguntar por suposiciones
*completar*

##### El simbolo pi

`pi` es un simbolo predefinido, por lo que es tenido en cuenta para ciertas simplificaciones:
```haskell
0**pi = 0 -- pi es positivo
sin(pi) = 0
```

### 2.2 Combinando expresiones

`Expr` es una instancia de las clases `Num`, `Fractional` y `Floating`, por lo que soporta las expresiones matematicas basicas y la aplicación de funciones.

```haskell
x = symbol "x"
y = symbol "y"

-- Sumas y restas
x+2 -- 2 es automaticamente casteado a un Expr, por lo que no es necesario fromNumber
x-9

-- Productos y divisiones
2*x
x/y

-- Potencias
x^2 -- para exponentes positivos de tipo 'Integer'
x^^5 -- para exponentes 'Integer' de cualquier signo
x**y -- potencia entre 'Expr'

-- Aplicar funciones
sin(x)
tan(9)+y
exp((4::Expr)) -- casting necesario, sino evaluaria a un Double
log(x-12*pi)

-- Tambien hay soporte para funciones anonimas, solo hay que pasar una lista con los argumentos
f = function "f"
f[x]+f[x] = 2*f(x)
```

Los elementos de tipo `Expr` cumplen todos los axiomas de cuerpo, excepto el de la propiedad distributiva:
```haskell
x*2 == 2*x -- True
(x+y)+9 == x+(y+9) -- True
2*(x+y) == 2*x+2*y -- True, los numeros se distribuyen
(x+y)*z == x*z + y*z -- False, los terminos no númericos no se distribuyen

1/0 == log(-1)
=>(autosimplifican a)
Undefined: division por cero == Undefined: logaritmo de un numero negativo
=>(la comparación evalua a)
True -- Todas las expresiones indefinidas, son iguales entre si
```

#### Autosimplificación

Las operaciones basicas ejecutan el proceso de **autosimplificación**, el cual realiza ciertas simplificaciones de manera automatica
```haskell
x+x ==> 2*x
x*x ==> x**2
(x**2)**3 ==> x**6
x + sin(pi/2) ==> x+1 -- sin(pi/2) = 1
```

La autosimplificación tambien se encarga de manejar expresiones que contengan terminos indefinidos:
```haskell
u = (1/0)::Expr -- Undefined: division por 0
v = symbol "v"
w = undefinedExpr "Undefined explicito"

sin(w)+1 ==> Undefined: Undefined explicito
u**u ==> Undefined: división por cero
u+v+w ==> Undefined: división por cero
w+v+u ==> Undefined: Undefined explicito -- Los indefinidos de mas a la izquierda tienen prioridad
```

Las funciones encargadas de realizar el procedimiento de autosimplificación se encuentran en el modulo `Expr.Simplify`, aunque nunca se usan en la practica, ya que son ejecutadas automaticamente por los operadores matematicos.

#### Detección de expresiones indefinidas

La autosimplificación permite detectar ciertas expresiones prohibidas, por ejemplo, aquellas que incluyen una división por 0
```haskell
1/(x-x) ==> Undefined: division por cero
1/(log(x/x)) ==> Undefined: division por cero
```


#### Limites de la autosimplificación

La **autosimplifiación** no realiza todas las simplificaciones posibles, primero porque la lista de reglas de simplificación puede ser muy larga y segundo porque una autosimplificación con muchas reglas podria interferir con el funcionamiento de otras funciones(ejemplo, si la autosimplificación aplicara la propiedad distributiva siempre que pudiera, seria imposible crear una función para factorizar polinomios):

Esto hace que algunas expresiones queden sin simplificar:
```haskell
sin(x)**2 + cos(x)**2 -- la expresión no cambia
1/(exp(2*x) - exp(x)**2) -- division por cero no reconocida
(x+1)**3 / (2*x**2+4*x+2) -- la expresión no cambia
```

Aun asi, muchas de estas simplificaciones pueden ser aplicadas usando los modulos especializados para simplificación.

```haskell
simplifyTrig (sin(x)**2 + cos(x)**2) ==> 1
expExpand (1/(exp(2*x) - exp(x)**2)) ==> Undefined: division por 0
rationalSimplify ((x+1)**3 / (2*x**2+4*x+2)) ==> x/2 + 1/2
```

### 2.3 Pattern Matching sobre Expr

El tipo `Expr` soporta *Pattern Matching*, esto permite analizar una expresión en base a su estructura y modificarla de la manera que sea necesaria:

```haskell
-- Extrae el primer operando de una expresión
primerOperando :: Expr -> Expr
primerOperando (Add (x :|| _)) = x
primerOperando (Mul (x :|| _)) = x
primerOperando (Pow x _) = x
primerOperando (Fun (x :| _)) = x
primerOperando x = x
```

#### Tipos TwoList y NonEmpty
Las expresiones en funciones se devuelven como un `NonEmpty Expr`, `NonEmpty a` representa una lista de elementos de tipo `a` que garantiza la existencia de al menos un elemento.
```haskell
data NonEmpty a = a :| [a]
```

Las expresiones en sumas y productos se devuelven en una `TwoList`, una `TwoList` es analoga a una `NonEmpty` pero garantiza la existencia de al menos 2 elementos.
```haskell
data TwoList a = a :|| NonEmpty a
```

Para el tipo `NonEmpty`: [consultar la documentación en Hackage](https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html)

### Patrones derivados
Los patrones basicos son `Number`, `Symbol`, `Add`, `Mul`, `Pow`, `Fun` y `Undefined`. Existen patrones adicionales que se derivan a partir de los basicos.

Para una implementación de los patrones, ver el archivo `Expr/Structure.hs`.

#### Listado de patrones implementados
| Patrón       | Descripción                                                                 |
|--------------|-----------------------------------------------------------------------------|
| `Number x`     | Matchea cualquier número `x`.                                                |
| `Symbol s`     | Matchea cualquier símbolo de nombre `s`.                                                  |
| `Add (x :\|\| y :\| xs )`        | Matchea una suma de dos o más expresiones. `x` es el primer elemento, `y` el segundo y `xs` es una lista con el resto de los argumentos.                                  |
| `Mul (x :\|\| y :\| xs )`        | Matchea un producto de dos o más expresiones. `x` es el primer elemento, `y` el segundo y `xs` es una lista con el resto de los argumentos.                                |
| `Pow x y`        | Matchea una potencia de base `x` y un exponente `y`.                            |
| `Fun f (x :\| xs)`        | Matchea una función aplicada a una lista de uno o más argumentos. `x` es el primer argumento y `xs` una lista con los argumentos restantes |
| `Undefined e`  | Matchea cualquier expresión indefinida, donde `e` es el error correspondiente  |
| `Pi`         | Matchea el simbolo pi |
| `Neg x`        | Matchea una expresión `x` multiplicada por un número negativo.|
| `MonomialTerm u n` | Matchea expresiones de la forma `u**n`, donde `u` es una expresión cualquiera y `n` es un numero natural mayo a 1|
| `Sqrt u`       | Matchea una expresión `u` elevada a `1/2` |
`Div n d` | Matchea una división de numerador `n` y denominador `d`.
| `Exp x`  | Matchea una expresión exponencial con base `e` y exponente `x`. |
| `Log x`  | Matchea una expresión logarítmica con base `e` y argumento `x`. |
| `Sin x`  | Matchea la función seno aplicada a `x`. |
| `Cos x`  | Matchea la función coseno aplicada a `x`. |
| `Tan x`  | Matchea la función tangente aplicada a `x`. |
| `Asin x` | Matchea la función arco seno aplicada a `x`. |
| `Acos x` | Matchea la función arco coseno aplicada a `x`. |
| `Atan x` | Matchea la función arco tangente aplicada a `x`. |
| `Derivative u x` | Matchea la derivada sin evaluar de `u` con respecto a `x`. |
| `Integral u x` | Matchea la integral indefinida sin evaluar de `u` con respecto a `x`. |
| `DefiniteIntegral u x a b` | Matchea la integral definida sin evaluar de `u` con respecto a `x` en el intervalo `[a, b]`. |


## 3. Modulos especiales
Los modulos especiales se construyen a partir del tipo `Expr` y permiten realizar las siguientes 4 funcionalidades:
- Evaluación numerica
- Simplificación avanzada
- Derivación
- Integración

### 3.1 Evaluación númerica
Para evaluar númericamente una expresión, hay que importar el modulo `Evaluate.Numeric`
```haskell
import Evaluate.Numeric
```
Las expresiones podran evaluarse usando la función `eval`:
```haskell
eval [] (2*sin(pi/4)) = 1.4142135623730951 -- sqrt 2
eval [(x,2.2)] (7.8+x) = 10-- Puedes reemplazar los simbolos por valores numericos
```
### 3.2 Simplificación avanzada
Los modulos para simplificación se encuentran en la carpeta "Simplification", estos permiten realizar la simplificación en 4 áreas:

- Expresiones algebraicas
- Expresiones trigonometricas
- Expresiones con exponenciales
- Expresiones con logaritmos

A su vez, todos los modulos(salvo los de expresiones algebraicas) contienen 3 funciones para realizar la simplificaciones, el funcionamiento exacto varia de modulo en modulo pero por lo general operan de la siguiente forma:
- Función expansión: Intenta hacer las expresiones mas grandes, puede llegar a formar expresiones mas pequeñas gracias a la autosimplificación:
    ```haskell
    -- Expansión algebraica
    expand((x + 1)*(x - 2) - (x - 1)*x) 
    =>(expande a) 
    x**2 - 2*x + x - 2 - x**2 + x
    =>(autosimplifica a)
    -2

    resultado final: -2
    ```

- Función de contración: Intenta hacer las expresiones mas pequeñas:
    ```haskell
    expContract (exp(2) * exp(5)) = exp(5) -- Contración de exponenciales
    ```

- Función de simplificación: Racionaliza la expresión, intenta contraer el numerador y el denominador y cancela terminos usando la autosimplificación:
    ```haskell
    rationaliSimplify ((x+y)*(x-y)/(x**3-x*y**2)) = 1/x
    ```

En general, la expansión no es la inversa ni de la contración ni de la simplificación, debido al proceso de autosimplificación:
```haskell
expExpand (expContract (exp(x)**2)) = exp(x)**2 
-- La expansión anuló la contración

expExpand (expContract (exp(x)**2- exp(2*x)))
=>(contrae a)
expExpand (exp(2*x) - exp(2*x))
=>(autosimplifica a)
expExpand 0
=>(expande a)
0
-- La expansión no anuló la contración
```


Los modulos que trabajan en cada área se destacan como sigue:
#### Simplificación algebraica
Es realizada por los modulos `Simplification.Algebraic` y `Simplification.Rationalize`
- Expansión: Realizada por la función `expand`, expande expresiones aplicando la propiedad distributiva y el binomio de Newton.
- Contracción: El equivalente a la contración algebraica es la factorización de polinomios, esta operación no esta soportada, sin embargo no es necesaria para la simplificación algebraica.
- Simplificación: Realizada por `rationalSimplify`, la simplificación se realizar usando el maximo común divisor entre polinomios.

#### Simplificación trigonometrica
Es realizada por los modulos `Simplification.Trigonometric`:
- Expansión: Realizada por la función `trigExpand`, expande los argumentos de los senos/cosenos aplicando la formula de la suma de angulos y la formula del angulo multiple.
- Contración: Realizada por la función `trigContract`, contrae los productos entre senos y cosenos y elimina potencias enteras de senos/cosenos.
- Simplificación: Realizada por `trigSimplify`, ver la documentación de la función para mas detalles de su funcionamiento.

#### Simplificación de exponenciales
Es realizada por el modulo `Simplification.Exponential` aplicando las siguientes propiedades:

1. $exp(x+y) = exp(x)*exp(y)$
2. $exp(xy)= exp(x)^y, \quad y \in \mathbb{Z}$

- Expansión: Realizada por `expExpand`, aplicando las propiedades 1 y 2 de izquierda a derecha.
- Contración: Realizada por `expContract` aplicando las propiedades 1 y 2 de derecha a izquierda.
- Simplificación: Realizada por `expSimplifiy`, contrayendo numerador y denominador de una expresión racionalizada.

#### Simplificación de logaritmos
Es realizada por el modulo `Simplification.Logarithm` aplicando las siguientes propiedades:

1. $\log(xy) = \log(x) + \log(y), \quad x,y \in \mathbb{R^+}$
2. $\log(x^n) = n*log(x), \quad x \in \mathbb{R^+}$

- Expansión: Realizada por `logExpand`, aplicando las propiedades 1 y 2 de izquierda a derecha.
- Contración: Realizada por `logContract` aplicando las propiedades 1 y 2 de derecha a izquierda.
- Simplificación: Realizada por `logSimplifiy`, contrayendo numerador y denominador de una expresión racionalizada.

### 3.3 Derivación
Para derivar expresiones, importar el modulo `Calculus.Derivate`:
```haskell
import Calculus.Derivate
```

Y luego usar la función `derivate`:
```haskell
derivate (x**2) x = 2*x
derivate (exp(x)) x = exp(x)
f = function "f"
g = function "g"
derivate (f[x]) = Derivate(f(x), x) -- Derivada desconocida, devuelvo una derivada sin evaluar
derivate (f[g[x]]) x = Derivate(f(x),g(x)) * Derivate(g(x), x) -- Derivada sin evaluar aplicando la regla de la cadena
```

### 3.4 Integración
Para integrar expresiones, importar el modulo `Calculus.Integrate`:
```haskell
import Calculus.Integrate
```

Y luego usar la función `integrate`:
```haskell
integrate (cos x) x = sin(x) -- Notar que no se agrega la constante de integración
integrate (exp(x)) = exp(x)
integrate (2*sin(x)*cos(x)) = -cos(x)^2
```

Si no se puede encontrar la integral de la función(ya sea porque no es una integral elemental, el algoritmo de integración no puede encontrarla o la misma se desconoce), se devuelve una integral desconocida:
```haskell
integrate (exp(-x**2)) x = Integral(e^(-x^2),x) -- Integral no elemental
integrate (1/(x**2+1)) x = Integral(1/(x^2+1),x) -- Integral elemental, pero no obtenida por el algoritmo
f = function "f"
integrate (f[x]) x = Integral(f(x), x) -- Integral desconocida
```

### 3.5 Parseo de expresiones
El modulo `Expr` viene incluida con la función `parseExpr` para parsear convertir una cadena de texto en una expresión

```haskell
parseExpr "x" -- Devuelve el simbolo x
parseExpr "x + x + sin(pi / 2)" -- Devuelve 2*x+1, la expresión se evalua usando autosimplificación
parseExpr "f(x) + g(y)" -- Devuelve f(x)+g(y), puede detectar funciones anonimas

a = symbol "a"
u = parseExpr "b+c"
a+u    -- Devuelve a+u, las expresiones parseadas pueden combinarse con expresiones no parseadas
```

En caso de un error de parseo, se devuelve una expresión indefinida.

```haskell
u = symbol "2++x" -- Undefined: Error de parseo
```

El parser se construye a partir de una gramatica de Happy, el archivo de la gramatica se encuentra en el modulo `Expr/Parser.y`.

### 3.6 PrettyPrinting
El prettyprinting de expresiones se realiza en el archivo `Expr/PrettyPrint.hs`, utilizando la libreria `PrettyPrinter`:
```haskell
y*2*x + y**2 + x**2 -- se muestra como x^2 + 2*x*y + y^2, los terminos se reorganizan
exp(x)+exp(y) -- se muestra como e^x+e^y
2 * x**(-1) * y**(-1) -- se muestra como 2/(x*y)
```

## 4. Organización de los archivos
La estructura del proyecto es la siguiente:
```
.
|-- src
|   |-- Calculus
|   |   |-- Derivate.hs -- Derivación de expresiones
|   |   |-- Integrate.hs -- Integración de expresiones
|   |   |-- Utils.hs
|   |-- Classes
|   |   |-- Assumptions.hs -- Funciones para suposiciones
|   |   |-- EvalResult.hs  -- Monada EvalResult
|   |-- Data
|   |   |-- Number.hs
|   |   |-- TriBool.hs -- Manejo de logica ternaria
|   |   |-- TwoList.hs
|   |-- Evaluate
|   |   |-- Numeric.hs
|   |-- Expr
|   |   |-- Expr.hs
|   |   |-- ExprType.hs -- Definición del tipo Expr
|   |   |-- Parser.y    -- Parser de expresiones
|   |   |-- PExpr.hs    
|   |   |-- PolyTools.hs -- Manejo de polinomios multivariables
|   |   |-- PrettyPrint.hs -- Prettyprinting de expresiones
|   |   |-- Simplify.hs -- Autosimplificación
|   |   |-- Structure.hs -- Pattern matching de expresiones
|   |-- Simplification
|   |   |-- Algebraic.hs
|   |   |-- Exponential.hs
|   |   |-- Logarithm.hs
|   |   |-- Rationalize.hs
|   |   |-- Trigonometric.hs
|-- CASkell.cabal
|-- README.md
```

## 5. Decisiones de diseño
### 5.1 EDSL por sobre DSL
La decisición inicial fue si crear un DSL (Domain Specific Language) o un EDSL (Embedded Domain Specific Language). Un DSL es un lenguaje de programación especializado en un dominio particular, mientras que un EDSL es un DSL que se construye dentro de un lenguaje de programación general, aprovechando su sintaxis y funcionalidades. 

Hay 2 razones por las que termine implementando un EDSL:

1. **Pattern Matching**: El pattern matching a la hora de trabajar con expresiones matemáticas es fundamental y es utilizado en la mayoría de las funciones del proyecto. El soporte de Haskell para realizar Pattern Matching, junto con las extensiones `PatternSynonyms` y `ViewPatterns`, resultó fundamental para simplificar y hacer más legible el código. En un DSL habria que implementar alguna forma de Pattern Matching desde 0, la cual seria potencialmente inferior a la de Haskell.
2. **Reutilización de la infraestructura de Haskell**: Al construir un EDSL dentro de Haskell, se puede aprovechar toda la infraestructura existente del lenguaje, incluyendo su sistema de tipos, funciones de alto orden, y librerías estándar. Esto reduce el esfuerzo de implementación y permite utilizar modulos especializados como `Happy` o `PrettyPrinter`.

### 5.2 Representación de expresiones
Internamente, las expresiones se representan como arboles de expresiones

![alt text](imgs/Arbol%20de%20imagenes.png)
Ejemplo de representación de $a/b+c-d$, sacada de *Computer alegebra and symbolic computation: Elementary Algorithms*

Estos arboles de expresiones se representan en codigo mediante el tipo `PExpr`, definido en `src/Expr/PExpr.hs`
```haskell
data PExpr = Number Number 
                | SymbolWithAssumptions String AssumptionsEnviroment
                | Mul [PExpr] 
                | Add [PExpr] 
                | Pow PExpr PExpr
                | Fun String [PExpr]
```

La expresión $a/b+c-d$ en `PExpr` seria similar a la siguiente
```haskell
a/b+c-d = Add [
        Mul [
            Symbol "a", 
            Pow (Symbol "b") (Number (-1))
        ],
        Symbol "c",
        Symbol "d"
    ]
```

Las funciones de autosimplificación operan con tipos `PExpr` y evaluan a una `Expr`, por ejemplo la función `simplifyPow` que realiza la simplificación de potencias.
```haskell
simplifyPow :: PExpr -> PExpr -> Expr
```

### 5.3 Manejo de errores con monadas
El tipo `PExpr` no realiza el manejo de expresiones indefinidas(`Undefined: ***`), sino que el mismo se realiza mediante el uso de monadas.

En particular se utiliza la monada `EvalResult`, la cual es una monada de error encapsulada.
```haskell
newtype EvalResult a = EvalResult { runEvalResult :: Either Error a }
```

El tipo `Expr` es simplemente una `PExpr` encapsulada en una monada `EvalResult`.
```haskell
type Expr = EvalResult PExpr
```

Los operadores matematicos se construyen utilizando la notación `do` y las funciones de autosimplificación, por ejemplo, la potenciación de expresiones:
```haskell
(**): Expr -> Expr -> Expr
a ** b = do
            a' <- a -- a' y b' son PExpr
            b' <- b
            simplifyPow a b 
```

Esto permite a los operadores matematicos detectar cuando trabajan con operadores indefinidos y actuar de manera acorde.

### 5.4 Suposiciones con logica ternaria
La autosimplificación necesita poder realizar suposiciones sobre las expresiones para hacer o no hacer simplificaciones. Estas suposiciones pueden ser verdaderas o falsas, pero no siempre se puede asignar alguno de estos dos valores.

Por ejemplo, en la expresión `0^x`. `x` es un simbolo con valor desconocido, por lo que no es posible asignar una suposición de positivo o negativo a `x`. Es decir los valores de verdad de `x>=0` o `x<=0` son desconocidos.

La logica ternaria aborda este problema, introduciendo un tercer valor de verdad a las expresiones booleanas, **Desconocido(U)**.

Esto permite que las suposiciones como `¿x es positivo?` tengan 3 posibles respuestas Verdadero(`T`), Falso(`F`) o Desconocido(`U`).

Las suposiciones se guardan directamente en el tipo `PExpr`, especificamente en las hojas de tipo `Symbol`. Para determinar el valor de verdad de una suposición sobre una expresión, se analiza el arbol de expresiones y se construye la suposición en base a los operandos involucrados(Ejemplo, si todos los operandos de una suma son positivos, la suma debe ser positiva).

Las operaciones para trabajar con valores de logica ternaria se encuentran en el archivo `Data/TriBool.hs`.

Mas información sobre logica ternaria: <https://en.wikipedia.org/wiki/Three-valued_logic>


## 6. Testing y documentación

Las funciones dentro del código cuentan con comentarios explicando la funcionalidad y el propósito. Junto con los comentarios se encuentran ejemplos de como se comporta la función.

```haskell
{-|
      Utiliza las reglas de derivación para calcular la derivada de una expresión con respecto a una variable dada.

      Las reglas de derivación utilizadas son:
  
      * Derivada de una constante: \(\dfrac{d}{dx}c = 0\)
      * Regla de la suma: \(\dfrac{d}{dx}\sum u_i = \sum \dfrac{du_i}{dx}\)

      ...

      === Ejemplos:
      >>> derivate (x**2 + 2*x + 1) x
      2*x+2
      >>> derivate (sin x) x
      Cos(x)

      ...
-}
derivate u x = ...
```

Estos comentarios pueden usarse para generar documentación del proyecto con `haddock`. Usando `make docs` se puede generar la documentación del codigo sin abrirla y usando `make open-docs` se puede generar la documentación y abrirla en el navegador web.

![*ejemplo de pagina de documentacion*](<imgs/ejemplo doc.png>)
*La pagina de documentacion para el ejemplo anterior*

Además, los ejemplos en la documentación se pueden usar para testear el funcionamiento correcto del proyecto. El comando `make test` lee los ejemplos de la documentación y los ejecuta para ver si obtienen el resultado esperado.


## 7. Bibilografia, librerías externas y referencias

### Bibliografia
- *Cohen, J. (2003). *Computer Algebra and Symbolic Computation: Elementary Algorithms*. A K Peters/CRC Press.*
- *Cohen, J. (2003). Computer Algebra and Symbolic Computation: Mathematical Methods. A K Peters/CRC Press.S. R., & Labahn, G. (1992).*
- *Meurer A, Smith CP, Paprocki M, Čertík O, Kirpichev SB, Rocklin M, Kumar A,
Ivanov S, Moore JK, Singh S, Rathnayake T, Vig S, Granger BE, Muller RP,
Bonazzi F, Gupta H, Vats S, Johansson F, Pedregosa F, Curry MJ, Terrel AR,
Roučka Š, Saboo A, Fernando I, Kulal S, Cimrman R, Scopatz A. (2017) SymPy:
symbolic computing in Python. *PeerJ Computer Science* 3:e103
https://doi.org/10.7717/peerj-cs.103*


### Librerías externas
- `base`: Provee las funcionalidades básicas del lenguaje Haskell.
- `pretty`: Provee herramientas para pretty-printing.
- `exact-combinatorics`: Provee funciones para hacer calculos combinatorios.
- `matrix`: Provee herramientas para trabajar con matrices.
- `happy`: Provee un generador de analizadores sintácticos para Haskell, utilizado para construir el parser de expresiones.


### Referencias
- Happy User Guide: <https://www.haskell.org/happy/doc/html/>
- Documentacion de PrettyPrinter: <https://hackage.haskell.org/package/pretty>
- Wikipedia, Three-valued logic: <https://en.wikipedia.org/wiki/Three-valued_logic>
- Sympy, un CAS implementado en Python. Usado como inspiración para el sistema de suposiciones ademas de una referencia de como deberian comportarse las funciones:
<https://www.sympy.org/en/index.html>