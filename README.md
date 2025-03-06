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
x+2 -- 2 es automaticamente casteado a un Expr, por lo que no es necesario fromNumber
x-9
x^2 -- para exponentes positivos de tipo 'Integer'
x^^5 -- para exponentes 'Integer' de cualquier signo
x**y -- potencia entre 'Expr'
x/y
sin(x)
tan(9)
exp(4)
log(x-12*pi)
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
