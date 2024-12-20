module Library where
import PdePreludat

type Palabra = String
type Verso = String -- SERIA COMO UNA ORACION
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

-- 1) Se pide

-- a) Determinar si dos palabras riman. Es decir, si generan una rima, ya sea asonante o consonante, 
-- pero teniendo en cuenta que dos palabras iguales no se consideran una rima.

--riman :: Palabra -> Palabra -> Bool
--riman palabra1 palabra2 = generanUnaRima palabra1 palabra2 && (palabra1 /= palabra2)

riman :: Palabra -> Palabra -> Bool
riman palabra1 palabra2 = (rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2) && (palabra1 /= palabra2)

generanUnaRima :: Palabra -> Palabra -> Bool
generanUnaRima palabra1 palabra2 
    | rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2 = True
    | otherwise                                                          = False

-- Rima asonante: se cumple cuando las dos últimas vocales de la palabra coinciden. Por ejemplo: parcial - estirar
rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante palabra1 palabra2 = ((== ultimasDosVocales palabra1) . ultimasDosVocales) palabra2

ultimasDosVocales :: Palabra -> String
ultimasDosVocales palabra = (reverse . take 2 . filter (esVocal) . reverse) palabra

-- Rima consonante: se cumple cuando las tres últimas letras de la palabra coinciden. Por ejemplo: función - canción
rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante palabra1 palabra2 = ((== ultimasTresPalabras palabra1) . ultimasTresLetras) palabra2

ultimasNLetras :: Number -> Palabra -> String
ultimasNLetras n palabra = (reverse . take n . reverse) palabra

ultimasNLetras' :: Number -> Palabra -> String
ultimasNLetras' n palabra = drop (length palabra - n) palabra

--ultimasTresLetras :: Palabra -> String
--ultimasTresLetras palabra = (reverse . take 3 . reverse) palabra

-- b) Enumerar todos los casos de test representativos (clases de equivalencia) de la función anterior. 
-- No hace falta escribir los tests (serían sólo sus nombres).

{-
Las clases de equivalencia son
  - Dos palabras riman por rima asonante
  - Dos palabras riman por rima consonante
  - Dos palabras iguales no riman
  - Dos palabras sin conexion no riman
-}

-- > riman "parcial" "estirar"  ---> True (rima asonante)
-- > riman "canción" "función"  ---> True (rima consonante)
-- > riman "arbol" "arbol"      ---> False (son iguales no riman)
-- > riman "arbol" "pecera"     ---> False (no cumple con ningun tipo de rima)

-- 2) Modelar las conjugaciones anteriores. Tener en cuenta que debe ser sencillo agregar más conjugaciones al sistema, 
-- ya que se planea hacerlo en próximas iteraciones.
-- Tip: Haskell tiene la función words que dado un string lo separa por espacios. Ejemplo:
-- > words "hola soy pepita"
-- ["hola","soy","pepita"]

-- CONJUGACIONES

-- Por medio de rimas: dos versos se conjugan con rima cuando logran rimar las últimas palabras de cada uno. Por ejemplo:
-- "no hace falta un programa que genere una canción"
-- "para saber que esto se resuelve con una función"

porMedioDeRimas :: Verso -> Verso -> Bool
porMedioDeRimas verso1 verso2 = riman ((last . words) verso1) ((last . words) verso2)

-- Haciendo anadiplosis: sucede cuando el segundo verso comienza con la misma palabra con la que termina el primero. Por ejemplo:
-- "este examen no se aprueba sin aplicación parcial"
-- "parcial lindo y divertido si rendiste todas las katas"

haciendoAnadiplosis :: Verso -> Verso -> Bool
haciendoAnadiplosis verso1 verso2 = (last . words) verso1 == (head . words) verso2 

-- PATRONES

-- 3) Se pide 
-- a) Modelar los patrones anteriores.

-- Simple: es un patrón en el que riman 2 versos, especificados por su posición en la estrofa. 

-- Por ejemplo, la siguiente estrofa tiene un patrón simple de 1 y 4, pero no de 1 y 3:
-- (1) esta rima es fácil como patear un penal
-- (2) solamente tiene como objetivo servir de ejemplo
-- (3) los versos del medio son medio fríos
-- (4) porque el remate se retoma al final

simple :: Number -> Number -> Estrofa -> Bool
simple primerPosicion segundaPosicion estrofa = porMedioDeRimas (estrofa !! (primerPosicion + 1)) (estrofa !! (segundaPosicion + 1))

