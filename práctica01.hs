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
