--Practica 01
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

--Definimos la función power para sacar la potencia de un número.
power :: Int -> Int -> Int
power n 0 = 1
power n 1 = n
power n m = n * (power n (m-1))

--lista [0, 1, 3, 7, 15, 31, 63]
lista1 :: [Int]
lista1 = [(power 2 x) - 1 | x <- [0..6]]

--lista [(3, 4),(7, 8),(11, 12),(15, 16), ...]
lista2 :: [Int]
lista2 = [((x-1), x), | x <- [1..], x*4]