-- Practica2.hs - v1.0                                                        --
--                           Lógica Computacional                             --
--                    Facultad de Ciencias, UNAM, 2018-2                      --
--                      Todos los derechos reservados.                        --
--                                                                            --
--                 Práctica 2: Tipos y lógica proposicional                   --

--Definiciones
--Tipo para las variables proposicionales.
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

--Tipo para las formulas proposicionales.
data Formula = Prop Var
|Neg Formula
|Formula :&: Formula
|Formula :|: Formula
|Formula :=>: Formula
|Formula :<=>: Formula deriving (Show, Eq, Ord)

-- Precedencia y asociatividad.
infixl 9 :&:
infixl 9 :|:
infixr 7 :=>:
infixl 8 :<=>:

-- PUNTO 1
-- Función recursiva que recibe una fórmula y devuelve el conjunto de variables
-- que hay en la fórmula.
varList :: Formula -> [Var]
varList F = [] --Creado para compilar.

-- PUNTO 2
-- Función que recibe una fórmula y devuelve su negación.
negacion :: Formula -> Formula
negacion proposicion = proposicion --Creado para compilar.

-- PUNTO 3
-- Función que recibe una fórmula y elimina implicaciones y equivalencias.
equivalencia:: Formula -> Formula
equivalencia proposicion = proposicion --Creado para compilar.

-- PUNTO 4
-- Función recursiva que recibe una fórmula proposicional y una lista de parejas
-- de variables proposicionales. Sustituye todas las presencias de variables en 
-- la fórmula por la pareja ordenada que le corresponde en la lista.
sustituye :: Formula -> [(Var,Var)] -> Formula
sustituye proposicion lista = proposicion --Creado para compilar.

-- PUNTO 5
-- Función recursiva que recibe una fórmula y una lista de parejas ordenadas de
-- variables con estados (True y False) y evalua la fórmula asignando el estado 
-- que le corresponde a cada variable.
interp :: Formula -> [(Var,Bool)] -> Bool
interp proposicion lista = proposicion --Creado para compilar.

-- PUNTO 6
-- Función que recibe una fórmula y devuelve la fórmula en Forma normal negativa.
fnn:: Formula -> Formula
fnn proposicion = proposicion --Creado para compilar.

-- PUNTO 7
-- Función que recibe una fórmula y devuelve la fórmula en Forma normal conjuntiva.
fnc:: Formula -> Formula
fnc proposicion = proposicion --Creado para compilar.

-- Fin Practica2.hs                                                           --
-- Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx       --
-- López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx    --
-- Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx           --
