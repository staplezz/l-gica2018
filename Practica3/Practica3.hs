--------------------------------------------------------------------------------
-- Practica3.hs - v1.0                                                        --
--                           Lógica Computacional                             --
--                    Facultad de Ciencias, UNAM, 2018-2                      --
--                      Todos los derechos reservados.                        --
--                                                                            --
--                 Práctica 3: Resolución Binaria                             --
--                                                                            --
-- Objetivos y Anotaciones                                                    --
--    Que el alumno implemente el algoritmo de resolución binaria.            --
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

--Función que recibe una lista y elimina los duplicados de la lista.
eliminaDup :: (Eq a) => [a] -> [a]
eliminaDup [] = []
eliminaDup (x:xs) = x:eliminaDup (filter (/= x) xs)

--  Función auxiliar de varList que realmente hace todo el trabajo
auxVarList :: Formula -> [Var]
auxVarList (Prop p) = [p]
auxVarList (Neg a)  = (auxVarList a)
auxVarList (a :&:   b) = (auxVarList a) ++ (auxVarList b)
auxVarList (a :|:   b) = (auxVarList a) ++ (auxVarList b)
auxVarList (a :=>:  b) = (auxVarList a) ++ (auxVarList b)
auxVarList (a :<=>: b) = (auxVarList a) ++ (auxVarList b)

-- Función recursiva que recibe una fórmula y devuelve el conjunto de variables
-- que hay en la fórmula.
varList :: Formula -> [Var]
varList a = eliminaDup (auxVarList a)

-- Función que recibe una fórmula y devuelve su negación.
negacion :: Formula -> Formula
negacion (Prop p) = Neg (Prop p)
negacion (Neg a)  = a
negacion (a :|: b) = (negacion a) :&: (negacion b)
negacion (a :&: b) = (negacion a) :|: (negacion b)
negacion a = negacion (equivalencia a)

-- Función que recibe una fórmula y elimina implicaciones y equivalencias.
equivalencia :: Formula -> Formula
equivalencia (Neg a) = negacion a
equivalencia (a :|:   b) = (equivalencia a) :|: (equivalencia b)
equivalencia (a :&:   b) = (equivalencia a) :&: (equivalencia b)
equivalencia (a :=>:  b) = ((negacion a) :|: (equivalencia b))
equivalencia (a :<=>: b) = ((negacion a) :|: (equivalencia b)) :&: 
                           ((negacion b) :|: (equivalencia a))
equivalencia a = a

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
sustituye (Neg a) l = Neg (sustituye a l)
sustituye (a :&:   b) l = (sustituye a l) :&:   (sustituye b l)
sustituye (a :|:   b) l = (sustituye a l) :|:   (sustituye b l)
sustituye (a :=>:  b) l = (sustituye a l) :=>:  (sustituye b l)
sustituye (a :<=>: b) l = (sustituye a l) :<=>: (sustituye b l)

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
interp (Neg a)  l = not (interp a l)
interp (a :=>:  b) l = (not (interp a l)) || (interp b l)
interp (a :<=>: b) l = (interp (a :=>: b) l) == (interp (b :=>: a) l)
interp (a :&: b) l = (interp a l) && (interp b l)
interp (a :|: b) l = (interp a l) || (interp b l)

-- PUNTO 1
-- Función que recibe una fórmula y el resultado es una lista de (2^n) pares 
-- ordenados, donde el primer elemento es un estado para cada una de las 
-- variables y el segundo elemento del par ordenado es el resultado de la 
-- función de interpretación de la fórmula en ese estado
tablaVerdad:: Formula -> [([(Var,Bool)], Bool)]
tablaVerdad p = [] -- Pa' compilar.

-- Funcion que dada una lista de interpretanciones, regresa True si es una 
-- tautología, Flase en otro caso.
auxEsTautologia:: [([(Var,Bool)], Bool)] -> Bool
auxEsTautologia [] = True
auxEsTautologia ((l,x):xs) = x && auxEsTautologia xs

-- PUNTO 2
-- Función que recibe una fórmula y devuelve True, si la fórmula es tautología y
-- False en otro caso.
esTautologia:: Formula -> Bool
esTautologia a = auxEsTautologia (tablaVerdad a)

-- Funcion que dada una lista de interpretanciones, regresa True si es una 
-- contradicción, Flase en otro caso.
auxEsContradicc:: [([(Var,Bool)], Bool)] -> Bool
auxEsContradicc [] = False
auxEsContradicc ((l,x):xs) = (not x) && (not (auxEsContradicc xs))

-- PUNTO 3
-- Función que recibe una fórmula y devuelve True, si la fórmula es contra-
-- dicción y False en otro caso.
esContradiccion:: Formula -> Bool
esContradiccion a = auxEsContradicc (tablaVerdad a)

-- PUNTO 4
-- Función que recibe una fórmula y devuelve True, si la fórmula es satisfacible
-- y False en otro caso.
esSatisfacible:: Formula -> Bool
esSatisfacible a = not (auxEsContradicc (tablaVerdad a))

-- Función que recibe una fórmula y la devuelve en Forma normal negativa.
fnn:: Formula -> Formula
fnn a = equivalencia a

-- Función auxiliar que distribuye conjunciones.
distriDisyun:: Formula -> Formula -> Formula
distriDisyun (a :&: b) g = (distriDisyun a g) :&: (distriDisyun b g)
distriDisyun a (b :&: g) = (distriDisyun a b) :&: (distriDisyun a g)
distriDisyun a b = a :|: b

-- Función auxilar de fnc que realmente hace todo.
auxFnc:: Formula -> Formula
auxFnc (a :&: b) = (auxFnc a) :&: (auxFnc b)
auxFnc (a :|: b) = distriDisyun (auxFnc a) (auxFnc b)
auxFnc a = a

-- Función que recibe una fórmula y la devuelve en Forma normal conjuntiva.
fnc:: Formula -> Formula
fnc a = auxFnc (equivalencia a)

-- Función que dada una formula de solo disyunciones regresa una lista de formu-
-- las atomicas en ella.
separaConjunciones:: Formula -> [Formula]
separaConjunciones (Prop p)  = [(Prop p)]
separaConjunciones (Neg  a)  = [(Neg  a)]
separaConjunciones (a :|: b) = (separaConjunciones a) ++ (separaConjunciones b)

-- Función que dada una formula en FNC regresa una lista de sus clausulas.
separaClausulas:: Formula -> [[Formula]]
separaClausulas (a :&: b) = (separaClausulas a) ++ (separaClausulas b)
separaClausulas a = [(separaConjunciones a)]

-- PUNTO 5
-- Función que calcula el conjunto S de cláusulas de una Fórmula.
calculaS:: Formula -> [[Formula]]
calculaS a = separaClausulas (fnc a)

-- PUNTO 6
-- Función que recibe dos cláusulas y devuelve el resolvente de ambas.
res:: [Formula] -> [Formula] -> [Formula]
res a b = []

saturacion:: [[Formula]] -> Bool
saturacion [] = False
saturacion (x:y:zs) = False

-- PUNTO 7
-- Función que indica si se obtiene la cláusula vacía después de aplicar el 
-- algoritmo de saturación a un conjunto de cláusulas.
resolucionBinaria:: Formula -> Bool
resolucionBinaria a = saturacion (calculaS a)

-- Funcion que dada una lista de formulas las transforma en una conjunción.
formulaLista:: [Formula] -> Formula
formulaLista (l:[]) = l
formulaLista (l:ls) = l :&: (formulaLista ls)

-- PUNTO 8
-- Función que recibe un conjunto de premisas, una conclusión y nos dice si el 
-- argumento lógico es correcto.
resBin:: [Formula] -> Formula -> Bool
resBin l a = resolucionBinaria ((formulaLista l) :&: (negacion a))

--------------------------------------------------------------------------------
-- Fin Practica3.hs                                                           --
--                                                                            --
-- Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx       --
-- López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx    --
-- Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx           --
--------------------------------------------------------------------------------