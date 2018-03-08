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
-- Función recursiva que recibe una fórmula y devuelve el conjunto de variables
-- que hay en la fórmula.
varList :: Formula -> [Var]
varList (formula) = eliminaDup (auxVarList formula)

auxVarList :: Formula -> [Var]
auxVarList (Prop p) = [p]
auxVarList (Neg fo) = (auxVarList fo)
auxVarList (for1 :&:   for2) = (auxVarList for1) ++ (auxVarList for2)
auxVarList (for1 :|:   for2) = (auxVarList for1) ++ (auxVarList for2)
auxVarList (for1 :=>:  for2) = (auxVarList for1) ++ (auxVarList for2)
auxVarList (for1 :<=>: for2) = (auxVarList for1) ++ (auxVarList for2)

eliminaDup :: (Eq a) => [a] -> [a]
eliminaDup [] = []
eliminaDup (x:xs) = x:eliminaDup (filter (/= x) xs)

-- PUNTO 2
-- Función que recibe una fórmula y devuelve su negación.
negacion :: Formula -> Formula
negacion (Prop p) = Neg (Prop p)
negacion (Neg (Prop p)) = Prop p
negacion (for1 :|: for2) = (negacion for1) :&: (negacion for2)
negacion (for1 :&: for2) = (negacion for1) :|: (negacion for2)
negacion formula = negacion (equivalencia formula)

-- PUNTO 3
-- Función que recibe una fórmula y elimina implicaciones y equivalencias.
equivalencia :: Formula -> Formula
equivalencia (Neg formula) = negacion formula
equivalencia (for1 :=>:  for2) = (negacion for1 :|: equivalencia for2)
equivalencia (for1 :<=>: for2) = (negacion for1 :|: equivalencia for2) :&: 
                                 (negacion for2 :|: equivalencia for1)
equivalencia formula = formula

-- PUNTO 4
-- Función recursiva que recibe una fórmula proposicional y una lista de parejas
-- de variables proposicionales. Sustituye todas las presencias de variables en 
-- la fórmula por la pareja ordenada que le corresponde en la lista.
sustituye :: Formula -> [(Var,Var)] -> Formula
sustituye (Prop p) l = busca p l
sustituye (Neg formula) l = Neg (sustituye formula l)
sustituye (for1 :&:   for2) l = (sustituye for1 l) :&:   (sustituye for2 l)
sustituye (for1 :|:   for2) l = (sustituye for1 l) :|:   (sustituye for2 l)
sustituye (for1 :=>:  for2) l = (sustituye for1 l) :=>:  (sustituye for2 l)
sustituye (for1 :<=>: for2) l = (sustituye for1 l) :<=>: (sustituye for2 l)

busca :: Var -> [(Var, Var)] -> Formula
busca p [] = Prop p
busca p ((x,y):zs) = if (p == x) 
                        then Prop y
                        else busca p zs

-- PUNTO 5
-- Función recursiva que recibe una fórmula y una lista de parejas ordenadas de
-- variables con estados (True y False) y evalua la fórmula asignando el estado 
-- que le corresponde a cada variable.
interp :: Formula -> [(Var,Bool)] -> Bool
interp proposicion lista = False --Creado para compilar.

-- PUNTO 6
-- Función que recibe una fórmula y la devuelve en Forma normal negativa.
fnn:: Formula -> Formula
fnn formula = equivalencia formula

-- PUNTO 7
-- Función que recibe una fórmula y la devuelve en Forma normal conjuntiva.
fnc:: Formula -> Formula
fnc formula = auxFnc (equivalencia formula)

auxFnc:: Formula -> Formula
auxFnc (Prop p) = Prop p
auxFnc (for1 :&: for2) = (auxFnc for1) :&: (auxFnc for2)
auxFnc (for1 :|: for2) = distriDisyun (auxFnc for1) (auxFnc for2)
auxFnc formula = formula

distriDisyun:: Formula -> Formula -> Formula
distriDisyun (Prop p) (Prop q) = Prop p :|: Prop q
distriDisyun (Neg (Prop p)) (Neg (Prop q)) = Neg (Prop p) :|: Neg (Prop q)
distriDisyun (Prop p :|: Prop q) (Prop s) = Prop p :|: Prop q :|: Prop s
distriDisyun (Prop s) (Prop p :|: Prop q) = Prop p :|: Prop q :|: Prop s
distriDisyun (for1 :&: for2) for3 = (distriDisyun for1 for3) :&: (distriDisyun for2 for3)
distriDisyun for1 (for2 :&: for3) = (distriDisyun for1 for2) :&: (distriDisyun for1 for3)

-- Fin Practica2.hs                                                           --
--                                                                            --
-- Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx       --
-- López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx    --
-- Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx           --
--------------------------------------------------------------------------------