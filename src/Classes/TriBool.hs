{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : TriBool
Description : Modulo para el manejo de logica ternaria, que soporta 3 valores de verdad, Verdadearo(T), Falso(F) y Desconocido(U).

La logica clasica soporta dos valores de verdad, Verdadero y Falso. Sin embargo, hay situaciones en las que no se puede 
determinar si una proposicion es verdadera o falsa. Por ejemplo, la expresion @0^x@ evalua a aiferentes valores dependiendo del valor de @x@.

@
    0 ^ x = 0, si x > 0
    0 ^ x = Undefined: "Division por 0", si x <= 0
@

Pero el valor de @x@ no siempre es conocido, por lo que el valor de verdad de @x > 0@ no siempre se puede inferir como verdadero o falso.

La logica ternaria aborda este problema, introduciendo un tercer valor de verdad, Desconocido (U). 
Este modulo proporciona una implementacion de la logica ternaria, que soporta los valores de verdad Verdadero (T), Falso (F) y Desconocido (U), 
ademas de una extensión de los operadores logicos para estos valores.

Mas información: <https://en.wikipedia.org/wiki/Three-valued_logic>

Ejemplo de uso:

@
import TriBool

-- and
T &&& T == T 
T &&& F == F
U &&& T == U

-- or
F ||| F = F 
F ||| U = U
T ||| U = T

@

Notar que las operaciones con valores desconocidos no siempre resultan en un valor desconocido.

-}
module TriBool where

-- | Tipo de dato que representa los valores de verdad, Verdadero, Falso y Desconocido.
data TriBool = 
    F   -- ^ Falso 
    | U -- ^ Desconocido
    | T -- ^ Verdadero
  deriving (
  Eq, 
  Ord, -- ^ F <= U <= T 
  Show)

-- = Eliminadores

-- | Retorna True si el valor es Verdadero, False en caso contrario.
true :: TriBool -> Bool
true T = True
true _ = False

-- | Retorna True si el valor es Falso, False en caso contrario.
false :: TriBool -> Bool
false F = True
false _ = False

-- | Retorna True si el valor es Desconocido, False en caso contrario.
unknown :: TriBool -> Bool
unknown U = True
unknown _ = False

-- | "if then else" para valores de verdad ternarios
triBoolCase :: TriBool -> a -> a -> a -> a
triBoolCase T t _ _ = t
triBoolCase F _ f _ = f
triBoolCase U _ _ u = u

---

-- | Convierte un valor de verdad booleano a uno ternario.
liftBool :: Bool -> TriBool
liftBool True = T
liftBool False = F

infixr 3 &&&
infixr 2 |||
infixr 3 !&&
infixr 2 /||

-- = Operadores logicos

-- | Clase que permite redefinir los operadores logicos para valores que no necesariamente son Booleanos, 
-- util para poder operar tanto con valores ternarios como booleanos.
class BAlgebra a b c | a b -> c where
    -- | Operador AND
    (&&&) :: a -> b -> c

    -- | Operador OR
    (|||) :: a -> b -> c

    -- | Operador NAND
    (!&&)  :: a -> b -> c

    -- | Operador XOR
    (/||) :: a -> b -> c

-- | Operador NOT
not3 :: BAlgebra b b c => b -> c
not3 p = p !&& p

-- | Definicion de los operadores logicos para valores booleanos, se comportan igual que sus pares de la logica de dos valores.
instance BAlgebra Bool Bool Bool where
    (&&&) = (&&)
    (|||) = (||)
    (!&&) = (&&) . not 
    (/||) = (/=)

-- | Definicion de los operadores logicos para valores ternarios.
instance BAlgebra TriBool TriBool TriBool where
    F &&& _ = F
    _ &&& F = F
    T &&& T = T
    _ &&& _ = U

    T ||| _ = T
    _ ||| T = T
    F ||| F = F
    _ ||| _ = U

    F !&& _ = T
    _ !&& F = T
    T !&& T = F
    _ !&& _ = U

    U /|| _ = U
    _ /|| U = U
    p /|| q = liftBool $ p /= q

-- | Definicion de los operadores logicos donde el primer operando es booleano y el segundo ternario
instance BAlgebra Bool TriBool TriBool where
    (&&&) = (&&&) . liftBool
    (|||) = (|||) . liftBool
    (!&&) = (!&&) . liftBool
    (/||) = (/||) . liftBool

-- | Definicion de los operadores logicos donde el primer operando es ternario y el segundo booleano
instance BAlgebra TriBool Bool TriBool where
    (&&&) = flip (&&&)
    (|||) = flip (|||)
    (!&&) = flip (!&&)
    (/||) = flip (/||)

-- = Operadores con listas

-- | Aplica un predicado a una lista de valores y retorna el XOR ternario de todos
xor3 :: (a -> TriBool) -> [a] -> TriBool
xor3 _ [] = F
xor3 f (x:xs)
    | unknown (f x) = U
    | false (f x) = xor3 f xs
    | otherwise = not3 $ xor3 f xs

-- | Aplica un predicado a una lista de valores y retorna el AND ternario de todos
and3 :: (a -> TriBool) -> [a] -> TriBool
and3 _ [] = T
and3 f (x:xs)
    | unknown (f x) = U
    | false (f x) = F
    | otherwise = and3 f xs

-- | Aplica un predicado a una lista de valores y retorna el OR ternario de todos
or3 :: (a -> TriBool) -> [a] -> TriBool
or3 _ [] = T
or3 f (x:xs)
    | unknown (f x) = U
    | true (f x) = T
    | otherwise = or3 f xs