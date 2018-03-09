--------------------------------------------------------------------------------
-- Practica2.hs - v1.0                                                        --
--                           Lógica Computacional                             --
--                    Facultad de Ciencias, UNAM, 2018-2                      --
--                      Todos los derechos reservados.                        --
--                                                                            --
--                 Práctica 2: Tipos y lógica proposicional                   --
--                                                                            --
-- Objetivos y Anotaciones                                                    --
--    Que el alumno aprenda a definir tipos en Haskell y lo aplique a la      --
--    lógica proposicional.                                                   --
--                                                                            --
-- Profesor: Pilar Selene Linares Arévalo                                     --
-- Ayudante: Daniela Calderón Pérez                                           --
-- Ayud.Lab: Alejando Hernández Mora                                          --
-- Ayud.Lab: Luis Manuel Martínez Damaso                                      --
--------------------------------------------------------------------------------

-- Tipo para las variables proposicionales.
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z 
    deriving (Show, Eq, Ord)

-- Tipo para las formulas proposicionales.
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
--Función que recibe una lista y elimina los duplicados de la lista.
eliminaDup :: (Eq a) => [a] -> [a]
eliminaDup [] = []
eliminaDup (x:xs) = x:eliminaDup (filter (/= x) xs)

--  Función auxiliar de varList que realmente hace todo el trabajo
auxVarList :: Formula -> [Var]
auxVarList (Prop p) = [p]
auxVarList (Neg formula) = (auxVarList formula)
auxVarList (alfa :&:   beta) = (auxVarList alfa) ++ (auxVarList beta)
auxVarList (alfa :|:   beta) = (auxVarList alfa) ++ (auxVarList beta)
auxVarList (alfa :=>:  beta) = (auxVarList alfa) ++ (auxVarList beta)
auxVarList (alfa :<=>: beta) = (auxVarList alfa) ++ (auxVarList beta)

-- Función recursiva que recibe una fórmula y devuelve el conjunto de variables
-- que hay en la fórmula.
varList :: Formula -> [Var]
varList (formula) = eliminaDup (auxVarList formula)

-- PUNTO 2
-- Función que recibe una fórmula y devuelve su negación.
negacion :: Formula -> Formula
negacion (Prop p) = Neg (Prop p)
negacion (Neg formula) = formula
negacion (alfa :|: beta) = (negacion alfa) :&: (negacion beta)
negacion (alfa :&: beta) = (negacion alfa) :|: (negacion beta)
negacion formula = negacion (equivalencia formula)

-- PUNTO 3
-- Función que recibe una fórmula y elimina implicaciones y equivalencias.
equivalencia :: Formula -> Formula
equivalencia (Neg formula) = negacion formula
equivalencia (alfa :|:   beta) = (equivalencia alfa) :|: (equivalencia beta)
equivalencia (alfa :&:   beta) = (equivalencia alfa) :&: (equivalencia beta)
equivalencia (alfa :=>:  beta) = ((negacion alfa) :|: (equivalencia beta))
equivalencia (alfa :<=>: beta) = ((negacion alfa) :|: (equivalencia beta)) :&: 
                                 ((negacion beta) :|: (equivalencia alfa))
equivalencia formula =  formula

-- PUNTO 4
-- Función auxilar de sustituye que busca una variable en la lista y regresa 
-- otra variable si hubo coincidencia.
busca :: Var -> [(Var, Var)] -> Formula
busca p [] = Prop p
busca p ((x,y):zs) = if (p == x) 
                        then Prop y
                        else busca p zs

-- Función recursiva que recibe una fórmula proposicional y una lista de parejas
-- de variables proposicionales. Sustituye todas las presencias de variables en 
-- la fórmula por la pareja ordenada que le corresponde en la lista.
sustituye :: Formula -> [(Var,Var)] -> Formula
sustituye (Prop p) l = busca p l
sustituye (Neg formula) l = Neg (sustituye formula l)
sustituye (alfa :&:   beta) l = (sustituye alfa l) :&:   (sustituye beta l)
sustituye (alfa :|:   beta) l = (sustituye alfa l) :|:   (sustituye beta l)
sustituye (alfa :=>:  beta) l = (sustituye alfa l) :=>:  (sustituye beta l)
sustituye (alfa :<=>: beta) l = (sustituye alfa l) :<=>: (sustituye beta l)

-- PUNTO 5
-- Función auxiliar de interp que dada una variable y una lista, regresa el 
-- valor boleano asignado a la variable en la lista.
buscaValor :: Var -> [(Var,Bool)] -> Bool
buscaValor prop [] = error "No todas las variables estan definidas"
buscaValor prop ((var, b):ys)
    | prop == var = b
    | otherwise = buscaValor prop ys

-- Función recursiva que recibe una fórmula y una lista de parejas ordenadas de
-- variables con estados (True y False) y evalua la fórmula asignando el estado 
-- que le corresponde a cada variable.
interp :: Formula -> [(Var,Bool)] -> Bool
interp (Prop p) l = buscaValor p l
interp (Neg formula) l = not (interp formula l)
interp (alfa :=>:  beta) l = (not (interp alfa l)) || (interp beta l)
interp (alfa :<=>: beta) l = (interp (alfa :=>: beta) l) == 
                             (interp (beta :=>: alfa) l)
interp (alfa :&: beta) l = (interp alfa l) && (interp beta l)
interp (alfa :|: beta) l = (interp alfa l) || (interp beta l)

-- PUNTO 6
-- Función que recibe una fórmula y la devuelve en Forma normal negativa.
fnn:: Formula -> Formula
fnn formula = equivalencia formula

-- PUNTO 7
-- Función auxiliar que distribuye conjunciones.
distriDisyun:: Formula -> Formula -> Formula
distriDisyun (alfa :&: beta) for3 = (distriDisyun alfa for3) :&:
                                    (distriDisyun beta for3)
distriDisyun alfa (beta :&: for3) = (distriDisyun alfa beta) :&:
                                    (distriDisyun alfa for3)
distriDisyun alfa beta = alfa :|: beta

-- Función auxilar de fnc que realmente hace todo.
auxFnc:: Formula -> Formula
auxFnc (alfa :&: beta) = (auxFnc alfa) :&: (auxFnc beta)
auxFnc (alfa :|: beta) = distriDisyun (auxFnc alfa) (auxFnc beta)
auxFnc formula = formula

-- Función que recibe una fórmula y la devuelve en Forma normal conjuntiva.
fnc:: Formula -> Formula
fnc formula = auxFnc (equivalencia formula)

-- Fin Practica2.hs                                                           --
--                                                                            --
-- Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx       --
-- López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx    --
-- Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx           --
--------------------------------------------------------------------------------