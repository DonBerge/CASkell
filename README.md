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

-- Tambien hay soporte para funciones anonimas
f = function "f"
f(x)+f(x) = 2*f(x)
```

#### Autosimplificación

Las operaciones basicas realizan el proceso de **autosimplificación**, el cual realiza ciertas simplificaciones de manera automatica
```haskell
x+x = 2*x
x*x = x**2
(x**2)**3 = x**6
x + sin(pi/2) = 1 -- sin(pi/2) = 1
```

#### Detección de expresiones indefinidas

Ademas los operadores pueden detectar ciertas expresiones prohibidas, por ejemplo, aquellas que incluyen una división por 0
```haskell
1/(x-x) = Undefined: division por cero
1/(log(x/x)) = Undefined: division por cero
```

Autosimplificación siempre se pueden detectar casos como este, ya que el problema de determinar si una expresión matematica es igual a 0 es indecidible. Aun asi, muchos de estos casos son detectados.


#### Limites de la autosimplificación

La **autosimplifiación** no realiza todas las simplificaciones posibles:
```haskell
sin(x)**2 + cos(x)**2 -- la expresión no cambia
1/(exp(2*x) - exp(x)**2) -- division por cero no reconocida
(x+1)**3 / (2*x**2+4*x+2) -- la expresión no cambia
```

Aun asi, muchas de estas simplificaciones pueden ser aplicadas usando los modulos especializados para simplificación.

```haskell
trigExpand (sin(x)**2 + cos(x)**2) -- 1
expExpand (1/(exp(2*x) - exp(x)**2)) -- Undefined: division por 0
rationalSimplify ((x+1)**3 / (2*x**2+4*x+2)) -- x/2 + 1/2
```

### 3.3 Modulos especiales
Los modulos especiales se construyen a partir del tipo `Expr` y permiten realizar las siguientes 4 funcionalidades:
- Evaluación numerica
- Simplificación avanzada
- Derivación
- Integración

#### Evaluación númerica
Para evaluar númericamente una expresión, hay que importar el modulo `Evaluate.Numeric`
```haskell
import Evaluate.Numeric
```
Las expresiones podran evaluarse usando la función `eval`:
```haskell
eval [] (2*sin(pi/4)) = 1.4142135623730951 -- sqrt 2
eval [(x,2.2)] (7.8+x) = 10-- Puedes reemplazar los simbolos por valores numericos
```
#### Simplificación avanzada
Los modulos para simplificación se encuentran en la carpeta "Simplification", estos permiten realizar la simplificación en 4 áreas:

- Expresiones algebraicas
- Expresiones trigonometricas
- Expresiones con exponenciales
- Expresiones con logaritmos

A su vez, todos los modulos(salvo los de expresiones algebraicas) contienen 3 funciones para realizar la simplificaciones, el funcionamiento exacto varia de modulo en modulo pero por lo general operan de la siguiente forma:
- Función expansión: Intenta hacer las expresiones mas grandes, puede llegar a formar expresiones mas pequeñas gracias a trabaja junto a la autosimplificación:
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
    *ejemplo de simplificación*
    ```

En general, la expansión no es la inversa ni de la contración ni de la simplificación, debido al proceso de autosimplificación
```haskell
expExpand (expContract (exp(x)**2)) = exp(x)**2 -- La expansión anulo la contración
expExpand (expContract (exp(x)**2- exp(2*x)))
=>(contrae a)
exp(2*x) - exp(2*x)
=>(autosimplifica a)
0
=>(expande a)
0
-- La expansión no anulo la contración
```


Los modulos que trabajan en cada área se destacan como sigue:
##### Simplificación algebraica
Es realizada por los modulos `Simplification.Algebraic` y `Simplification.Rationalize`
- Expansión: Realizada por la función `expand`, expande expresiones aplicando la propiedad distributiva y el binomio de Newton.
- Contracción: El equivalente a la contración algebraica es la factorización de polinomios, esta operación no esta soportada, sin embargo no es necesaria para la simplificación algebraica.
- Simplificación: Realizada por `rationalSimplify`, la simplificación se realizar usando el maximo común divisor entre polinomios.

##### Simplificación trigonometrica
Es realizada por los modulos `Simplification.Trigonometric`:
- Expansión: Realizada por la función `trigExpand`, expande los argumentos de los senos/cosenos aplicando la formula de la suma de angulos y la formula del angulo multiple.
- Contración: Realizada por la función `trigContract`, contrae los productos entre senos y cosenos y elimina potencias enteras de senos/cosenos.
- Simplificación: Realizada por `trigSimplify`, ver la documentación de la función para mas detalles de su funcionamiento.

##### Simplificación de exponenciales
Es realizada por el modulo `Simplification.Exponential` aplicando las siguientes propiedades:

1. `exp(x+y) = exp(x)*exp(y)`
2. `exp(x*y)= exp(x)^y`

- Expansión: Realizada por `expExpand`, aplicando las propiedades 1 y 2 de izquierda a derecha.
- Contración: Realizada por `expContract` aplicando las propiedades 1 y 2 de derecha a izquierda.
- Simplificación: Realizada por `expSimplifiy`, contrayendo numerador y denominador de una expresión racionalizada.

##### Simplificación de logaritmos
Es realizada por el modulo `Simplification.Logarithm` aplicando las siguientes propiedades:

1. `log(x*y) = log(x) + log(y)`
2. `log(x^y) = y*log(x)`

- Expansión: Realizada por `logExpand`, aplicando las propiedades 1 y 2 de izquierda a derecha.
- Contración: Realizada por `logContract` aplicando las propiedades 1 y 2 de derecha a izquierda.
- Simplificación: Realizada por `logSimplifiy`, contrayendo numerador y denominador de una expresión racionalizada.