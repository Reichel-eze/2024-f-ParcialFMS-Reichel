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

--cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
--cumplen f comp v1 v2 = comp (f v1) (f v2)

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f op v1 v2 = f v1 `op` f v2

-- 1) Se pide

-- a) Determinar si dos palabras riman. Es decir, si generan una rima, ya sea asonante o consonante, 
-- pero teniendo en cuenta que dos palabras iguales no se consideran una rima.

--riman :: Palabra -> Palabra -> Bool
--riman palabra1 palabra2 = generanUnaRima palabra1 palabra2 && (palabra1 /= palabra2)

riman :: Palabra -> Palabra -> Bool
riman palabra1 palabra2 = (rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2) && (palabra1 /= palabra2)

--generanUnaRima :: Palabra -> Palabra -> Bool
--generanUnaRima palabra1 palabra2 
--    | rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2 = True
--    | otherwise                                                          = False

-- Rima asonante: se cumple cuando las dos últimas vocales de la palabra coinciden. Por ejemplo: parcial - estirar
--rimaAsonante :: Palabra -> Palabra -> Bool
--rimaAsonante palabra1 palabra2 = ((== ultimasDosVocales palabra1) . ultimasDosVocales) palabra2

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante = cumplen (ultimasNVocales 2) (==)

ultimasNVocales :: Number -> Palabra -> String
ultimasNVocales n = ultimasNLetras n . filter vocal

vocal :: Char -> Bool
vocal letra = esVocal letra || tieneTilde letra

--ultimasDosVocales :: Palabra -> String
--ultimasDosVocales palabra = (reverse . take 2 . filter (esVocal) . reverse) palabra

-- Rima consonante: se cumple cuando las tres últimas letras de la palabra coinciden. Por ejemplo: función - canción
--rimaConsonante :: Palabra -> Palabra -> Bool
--rimaConsonante palabra1 palabra2 = ((== ultimasTresPalabras palabra1) . ultimasTresLetras) palabra2

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante = cumplen (ultimasNLetras 3) (==)

ultimasNLetras :: Number -> Palabra -> String
ultimasNLetras n = reverse . take n . reverse 

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

type Conjugacion = Verso -> Verso -> Bool

-- Por medio de rimas: dos versos se conjugan con rima cuando logran rimar las últimas palabras de cada uno. Por ejemplo:
-- "no hace falta un programa que genere una canción"
-- "para saber que esto se resuelve con una función"

--porMedioDeRimas :: Verso -> Verso -> Bool
--porMedioDeRimas verso1 verso2 = riman ((last . words) verso1) ((last . words) verso2)

porMedioDeRimas :: Verso -> Verso -> Bool
porMedioDeRimas = cumplen ultimaPalabra riman

ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words

-- Haciendo anadiplosis: sucede cuando el segundo verso comienza con la misma palabra con la que termina el primero. Por ejemplo:
-- "este examen no se aprueba sin aplicación parcial"
-- "parcial lindo y divertido si rendiste todas las katas"

--haciendoAnadiplosis :: Verso -> Verso -> Bool
--haciendoAnadiplosis verso1 verso2 = (last . words) verso1 == (head . words) verso2 

haciendoAnadiplosis :: Verso -> Verso -> Bool
haciendoAnadiplosis verso1 verso2 = ultimaPalabra verso1 == primeraPalabra verso2

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words

-- PATRONES

-- 3) Se pide 
-- a) Modelar los patrones anteriores.

-- Simple: es un patrón en el que riman 2 versos, especificados por su posición en la estrofa. 

-- Por ejemplo, la siguiente estrofa tiene un patrón simple de 1 y 4, pero no de 1 y 3:

-- ["esta rima es fácil como patear un penal", "solamente tiene como objetivo servir de ejemplo", "los versos del medio son medio fríos", "porque el remate se retoma al final"]

-- (1) esta rima es fácil como patear un penal
-- (2) solamente tiene como objetivo servir de ejemplo
-- (3) los versos del medio son medio fríos
-- (4) porque el remate se retoma al final

type Par = (Number, Number)

type Patron = Estrofa -> Bool

simple :: Par -> Estrofa -> Bool
simple (n1,n2) estrofa = porMedioDeRimas (posicionVerso n1 estrofa) (posicionVerso n2 estrofa)

posicionVerso :: Number -> Estrofa -> Verso
posicionVerso n estrofa = estrofa !! (n-1)

-- Esdrújulas: Todos los versos terminan con palabras en esdrújula. Diremos que una palabra es esdrújula cuando la 
-- antepenúltima vocal está acentuada. Un ejemplo de este patrón sería:

esdrujulas :: Estrofa -> Bool
esdrujulas = all terminaConEsdrujula

terminaConEsdrujula :: Verso -> Bool
terminaConEsdrujula = esEsdrujula . ultimaPalabra

esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde . antePenultimaVocal 

antePenultimaVocal :: String -> Char
antePenultimaVocal = head . ultimasNVocales 3

-- Anáfora: Todos los versos comienzan con la misma palabra. Por ejemplo:

anafora :: Estrofa -> Bool
anafora = iguales . map primeraPalabra

iguales :: [String] -> Bool
iguales []                   = False
iguales (palabra : palabras) = all (== palabra) palabras 

--igualesV2 :: [String] -> Bool
--igualesV2 []                   = False
--igualesV2 (palabra1 : palabra2 : palabras) = palabra1 == palabra2 && igualesV2 (palabra2:palabras) 

-- Cadena: Es un patrón que se crea al conjugar cada verso con el siguiente, usando siempre la misma conjugación. 
-- La conjugación usada es elegida por el artista mientras está rapeando. Por ejemplo, una cadena de anadiplosis sería:
-- Tip: puede hacerse utilizando recursividad.

cadena :: Conjugacion -> Estrofa -> Bool
cadena _ []  = False
cadena _ [_] = True
cadena conjugacion (verso1:verso2:versos) = conjugacion verso1 verso2 && cadena conjugacion (verso2:versos)

-- CombinaDos: Dos patrones cualesquiera se pueden combinar para crear un patrón más complejo, y 
-- decimos que una estrofa lo cumple cuando cumple ambos patrones a la vez. Por ejemplo, si contemplamos el patrón 
-- combinado de esdrújulas y anáfora, una estrofa que cumpliría podría ser:

combinaDos :: Patron -> Patron -> Estrofa -> Bool
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

-- b) Usar el punto a para definir los siguientes patrones combinados:

-- aabb = patrón simple entre 1 y 2 + otro simple entre 3 y 4
-- abab = patrón simple entre 1 y 3 + otro simple entre 2 y 4
-- abba = patrón simple entre 1 y 4 + otro simple entre 2 y 3
-- hardcore = patrón de cadena de rimas + esdrújulas

aabb :: Patron
aabb = simple (1,2) `combinaDos` simple (3,4)

abab :: Patron
abab = simple (1,3) `combinaDos` simple (2,4)

abba :: Patron
abba = simple (1,4) `combinaDos` simple (2,3)

hardcore :: Patron
hardcore = cadena porMedioDeRimas `combinaDos` esdrujulas

-- c) ¿Se podría saber si una estrofa con infinitos versos cumple con el patrón hardcore? 
-- ¿Y el aabb? Justifique en cada caso específicamente por qué (no valen respuestas genéricas).

-- Si tengo una estrofa con infinitos versos y quiero analizar si cumple con el patron hardcore (patrón de cadena de 
-- rimas + esdrújulas). Si quisiera verificar si cada uno de las versos rima con el siguienteo o que cada uno termina con 
-- esdrujula NO podria hacerlo. Por lo tanto el caso de verificar un resultado como True es imposible de obtener (aunque asi
-- nos lo garanticen). Lo que si se podria llegar es a un caso de False ya que con solo comprobar que algun verso no rima
-- con el siguiente o que algun verso no termine con esdrujula ya seria suficiente para retornar un False (sin ser necesario 
-- seguir recorriendo la lista)   

-- La razon es porque ya con que uno de los patrones de hardcore NO se cumpla, Haskell no seguira avanzando en la lista inifinita 
-- de versos ya que cuenta con una evaluacion del tipo perezosa (lazy evaluation). 
-- Entonces, por ejemplo si los dos primeros dos versos NO conjugan por medio de rimas, entonces NO es necesario seguir 
-- buscando en la lista
-- Otro ejemplo seria, si el primer verso tiene su ultima palabra que NO es esdrujula, entonces la estrofa no cumple con el
-- patron esdrujulas (ya si un verso no cumple, entonces TODOS no cumplen)

-- Para el caso del aabbb. En este caso es posible retonar ambos resulados (tanto True como False) porque el algoritmo se va a 
-- concentrar exclusivamente en los pares pasados (1,2 y 3,4), por lo tanto a los demas versos de la estrofa no los analizara
-- Todo esto gracias a la evaluacion perezosa (lazy evaluation) que utiliza Haskell, simplemente evaluare los versos que sean
-- necesarios para ver si cumplen con el patron simple y en su debido caso retornar True o False
