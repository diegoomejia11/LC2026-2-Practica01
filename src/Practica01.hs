module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square s) = s * s
area (Rectangle b h) = b * h
area (Triangle s) = (sqrt 3 / 4) * s * s
area (Trapeze b1 b2 h) = ((b1 + b2) / 2) * h    

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter = undefined

--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = Shape

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance = undefined 

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 = undefined

--Ejercicio 3
data Haskellium = Undefined

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son = undefined

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo [] = True
palindromo [_] = True
-- comparamos la cabeza de la cadena con la cola, usamos && para que
-- de ser igual la cabeza y la cola se llama a la función nuevamente 
-- en la sig llamada comienza desde el siguiente elemento del string del inicio y el final
palindromo xs = (head xs == last xs) && palindromo (init(tail xs))


--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
-- Ocupamos una función, un valor inicial(base) y una lista sobre la cual trabajaremos
-- Se ejecuta la función en el primer elemento y se realiza la llamada recursiva para el resto
myFoldr f z (x:xs) = f x (myFoldr f z (xs))

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
-- El conjuto potencia de la lista vacia es una lista con la lista vacia como elemento
conjuntoPotencia [] =[[]] 
-- Genera todas las listas que contienen a x y lo concatenamos con las listas que no contienen a x como elemento
conjuntoPotencia (x:xs) = [x:ys| ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs 

--conjuntoPotencia = 
--ARBOLES

--Implementacion

data OneTwoTree a = Undefinedd

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined

-- Commit jala?