--------------------------------------------------------------------------------
-- Practica1.hs - v1.0                                                        --
--                           Lógica Computacional                             --
--                    Facultad de Ciencias, UNAM, 2018-2                      --
--                      Todos los derechos reservados.                        --
--                                                                            --
--                               Práctica 1                                   --
--                                                                            --
-- Objetivos y Anotaciones                                                    --
--    Que el alumno tenga una breve introducción a Haskell y la programación  --
--    funcional, así como a utilizar los tipos básicos en Haskell,            --
--    estructuras de control y también la caza de patrones.                   --
--                                                                            --
-- Profesor: Pilar Selene Linares Arévalo                                     --
-- Ayudante: Daniela Calderón Pérez                                           --
-- Ayud.Lab: Alejando Hernández Mora                                          --
-- Ayud.Lab: Luis Manuel Martínez Damaso                                      --
--------------------------------------------------------------------------------

--PUNTO 1
--Evalúa a b c x en la derivada de ax^2+bx+c.
deriva:: Int -> Int -> Int -> Int -> Int
deriva a b c x = (2*a*x) + b

--PUNTO 2
--Calcula el área de un cilindro.
areaCilindro:: Float -> Float -> Float
areaCilindro r h = 2*pi*r*(r + h)

--Calcula el volúmen de un cilindro.
volumenCilindro:: Float -> Float -> Float
volumenCilindro r h = 2*pi*r^2*h

--PUNTO 3
--Realiza operaciones varias dependiendo de el caractér que le pasan.
aplicaOperacion:: Char -> Int -> Int -> Int
aplicaOperacion a b c =
    case a of
        's' -> b
        't' -> c
        'a' -> b   +    c
        'r' -> b   -    c
        'p' -> b   *    c
        'd' -> b `div`  c
        'e' -> b   ^    c

--PUNTO 4
--Recibe un entero y devuelve la aproximación entera a su raíz cuadrada.
raizEntera :: Int -> Int
raizEntera n = aux n
  where
    aux x
      | x*x > n = aux (x - 1)
      | otherwise = x

--PUNTO 5,6
--Recibe un entero n y devuelve la suma de los primeros n números naturales
sumaNat:: Int -> Int
sumaNat n = (n*(n+1)) `div` 2

--PUNTO 7
--Función que recibe un número n y devuelve la lista con los primeros n términos
--de la sucesión tribonacci.
tribonaccies:: Int -> [Int]
tribonaccies n = map tribonacciesAux [0, 1.. (n-1)]

tribonacciesAux:: Int -> Int
tribonacciesAux 0 = 0
tribonacciesAux 1 = 1
tribonacciesAux 2 = 1
tribonacciesAux n = tribonacciesAux (n-3) + 
                    tribonacciesAux (n-2) + 
                    tribonacciesAux (n-1)

--Función que recibe una lista y elimina los duplicados adyacentes de la lista,
--dejando una presencia de cada elemento contiguo.
eliminaDup:: (Eq a) => [a] -> [a]
eliminaDup ls = eliminaDupAux ls

eliminaDupAux:: Eq a => [a] -> [a]
eliminaDupAux []        =   []
eliminaDupAux [x]       =   [x]
eliminaDupAux (x:y:zs)  =   if (x == y)
                            then eliminaDupAux(x:zs)
                            else x:eliminaDupAux(y:zs)

--PUNTO 8
--Una función que recibe una lista y devuelve la misma pero en el orden inverso.
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

--Función que recibe una lista y devuelve a los elementos de la lista que
--cumplen el predicado recibido..
filtra :: (a -> Bool) -> [a] -> [a]
filtra predicado []  =  []  
filtra predicado [x] =  if predicado x
                        then [x]
                        else []
filtra predicado (x : xs) = if predicado x 
                            then (x : filtra predicado xs)
                            else (filtra predicado xs)

--Función que recibe una lista y devuelve una lista con pares ordenados (k, x),
--donde k es el máximo número de apariciones consecutivas del elemento x.
apariciones :: (Eq a) => [a] -> [(Int,a)]
apariciones lista = quita [] ( reversa (cuenta lista []))

--Cuenta repeticiones cotinuas de un elemento y lo guarda en duplas.
cuenta :: (Eq a) => [a] -> [(Int,a)] -> [(Int,a)]
cuenta [] resultado = resultado
cuenta (x:xs) [] = cuenta xs [(1,x)]
cuenta (x:xs) ((i,a):ys) = if x == a then
                                cuenta xs ((i+1,a):ys)
                            else
                                cuenta xs ((1,x):((i,a):ys))

--Quita las aparaciones menores de una dupla con contador como 1er elemento. 
quita :: (Eq a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
quita sal [] = reversa sal
quita sal ((i,a):ls) = quita ((n,a):sal) (filtra (\(j,b) -> b/=a) ((i,a):ls))
    where n = maximo (filtra(\(j,b) -> b==a) ls) i
    
--De una lista de duplas, nos da el número más grande en la primer entrada.
maximo :: [(Int,a)] -> Int -> Int
maximo [] n = n
maximo ((i,a):ls) n =  if i>n then
                        maximo ls i
                    else
                        maximo ls n
--PUNTO 9
--Lista [0, 1, 3, 7, 15, 31, 63]
lista1 = [(2^x) - 1 | x <- [0..6]]

--Lista [(3, 4),(7, 8),(11, 12),(15, 16), ...]
lista2 = [((x-1),x) | x <- [4,8..]]

--------------------------------------------------------------------------------
-- Fin Practica1.hs                                                           --
--                                                                            --
-- Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx       --
-- López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx    --
-- Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx           --
--------------------------------------------------------------------------------