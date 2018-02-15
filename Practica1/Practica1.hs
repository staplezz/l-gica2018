--Practica 1
--Lógica Computacional 2018.

--Evalúa a b c x en la derivada de ax^2+bx+c.
deriva:: Int -> Int -> Int -> Int -> Int
deriva a b c x = (2*a*x) + b

--Calcula el área de un cilindro.
areaCilindro:: Float -> Float -> Float
areaCilindro r h = 2*pi*r*(r + h)

--Calcula el volúmen de un cilindro.
volumenCilindro:: Float -> Float -> Float
volumenCilindro r h = 2*pi*r^2*h

--Realiza operaciones varias dependiendo de el caractér que le pasan.
aplicaOperacion:: Char -> Int -> Int -> Int
aplicaOperacion a b c =
	case a of
		's' -> b
		't' -> c
		'a' -> b + c
		'r' -> b - c
		'p' -> b*c
		'd' -> b `div` c
		'e' -> b^c

--Recibe un entero y devuelve la aproximación entera a su raíz cuadrada.
raizEntera:: Int -> Int
raizEntera r =
	round ( sqrt (fromIntegral r))

--Recibe un entero n y devuelve la suma de los primeros n números naturales
sumaNat:: Int -> Int
sumaNat 1 = 1
sumaNat n = n + sumaNat (n- 1)

tribonaccies:: Int -> [Int]
tribonaccies n = []
--TODO

eliminaDup:: [a] -> [a]
eliminaDup ls = ls
--TODO

--PUNTO 8
--Una función que recibe una lista y devuelve la misma pero en el orden inverso.
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

--Una función que recibe una lista y devuelve a los elementos de la lista que
--cumplen el predicado recibido (filter).
filtra :: (a -> Bool) -> [a] -> [a]
filtra predicado []  = []	
filtra predicado [x] = if predicado x then [x] else []
filtra predicado (x : xs) = if predicado x 
							then ([x] ++ filtra predicado xs)
							else (filtra predicado xs)

apariciones :: [a] -> [(Int,a)]
apariciones ls = [(1,head ls)]
--TODO

--PUNTO 9
--Lista [0, 1, 3, 7, 15, 31, 63]
lista1 = [(2^x) - 1 | x <- [0..6]]

--Lista [(3, 4),(7, 8),(11, 12),(15, 16), ...]
lista2 = [((x-1),x) | x <- [4,8..]]