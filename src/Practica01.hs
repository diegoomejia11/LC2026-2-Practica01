module Practica01 where

-- TIPOS ALGEBRAICOS

-- Ejercicio 1
data Shape
  = Circle Float -- representa el radio
  | Square Float -- representa un lado
  | Rectangle Float Float -- representa base y altura
  | Triangle Float -- representa un lado
  | Trapeze Float Float Float -- representa base mayor, base menor y altura
  deriving (Show, Eq)

-- Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square s) = s * s
area (Rectangle b h) = b * h
area (Triangle s) = (sqrt 3 / 4) * s * s
area (Trapeze b1 b2 h) = ((b1 + b2) / 2) * h

-- Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square s) = 4 * s
perimeter (Rectangle b h) = 2 * (b + h)
perimeter (Triangle s) = 3 * s
perimeter (Trapeze b1 b2 h) = 2 * (sqrt (((b1 - b2) / 2) ** 2 + h ** 2)) + b1 + b2

type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

-- Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 p = distance p (0, 0)

-- Ejercicio 3
data Haskellium = Haskellium
  { name :: String,
    lastName1 :: String,
    lastName2 :: String,
    location :: Point,
    houseShape :: Shape
  }
  deriving (Show, Eq)

-- Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son father mother newName =
  Haskellium
    { name = newName,
      lastName1 = lastName1 father,
      lastName2 = lastName1 mother,
      location = location father,
      houseShape = houseShape father
    }

-- Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost h = case houseShape h of
  Square s -> 2 * area (Square s)
  Rectangle b height -> 1.875 * area (Rectangle b height)
  _shape -> 0 -- lo use con el _ porque no se me ocurrio como usar los otros casos y haskell me marcaba error
  -- Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo

timeToWork :: Haskellium -> Float
timeToWork h = from0 (location h) / 30

-- LISTAS Y FUNCIONES
-- Ejercicio 1
palindromo :: String -> Bool
palindromo [] = True
palindromo [_] = True
-- comparamos la cabeza de la cadena con la cola, usamos && para que
-- de ser igual la cabeza y la cola se llama a la función nuevamente
-- en la sig llamada comienza desde el siguiente elemento del string del inicio y el final
palindromo xs = (head xs == last xs) && palindromo (init (tail xs))

-- Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
-- Ocupamos una función, un valor inicial(base) y una lista sobre la cual trabajaremos
-- Se ejecuta la función en el primer elemento y se realiza la llamada recursiva para el resto
myFoldr f z (x : xs) = f x (myFoldr f z (xs))

-- Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
-- El conjuto potencia de la lista vacia es una lista con la lista vacia como elemento
conjuntoPotencia [] = [[]]
-- Genera todas las listas que contienen a x y lo concatenamos con las listas que no contienen a x como elemento
conjuntoPotencia (x : xs) = [x : ys | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

-- ARBOLES

-- Implementacion

data OneTwoTree a
  = Void
  | Node a (OneTwoTree a)
  | Branch a (OneTwoTree a) (OneTwoTree a)
  deriving (Show, Eq)

-- Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node x t) = x + suma t
suma (Branch x t1 t2) = x + suma t1 + suma t2
