Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx
López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx
Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx

Ésta implementación fue creada para SWI-Prolog version 7.4.2.

Pruebas unitarias recomendadas: 

Ejercicio 1.a) Probamos push.
push(x, [], X).
    X = [x].
push(x, [y,z], X).
    X = [x, y, z].

Ejercicio 1.b) Probamos pop.
pop([],X).
    X = [].
pop([x],X).
    X = [].
pop([x,y,z],X).
    X = [y, z].

Ejercicio 1.c) Probamos append.
append([],[x,y,z],X).
X = [x, y, z].
append([a,b,c],[],X).
X = [a, b, c].
append([a,b,c],[x,y,z],X).
X = [a, b, c, x, y, z].

Ejercicio 1.d) Probamos listSum.
listSum([1],X).
    X = 1.
listSum([],X).
    X = 0.
listSum([1,2,3,4],X).
    X = 10 .

Ejercicio 2.a) Verificamos los conjuntos del autómata.

El alfabeto:
alfabeto(X).  
    X = a ;
    X = b.

Los estados:
estado(X).
    X = 1 ;
    X = 2 ;
    X = 3.

Los estados iniciales:
inicial(X).
    X = 1.

Los estados finales:
final(X).
    X = 3.

Las transiciones:
delta(X,Y,Z).
    X = 1, Y = b, Z = 2 ;
    X = Z, Z = 2, Y = a ;
    X = 2, Y = a, Z = 3 ;
    X = 3, Y = b, Z = 2.

Ejercicio 2.b) Verificamos el autómata.

acepta([a],2).
    true.
acepta([b,a,b,a,a,b,a,a,a],1).
    true .
acepta([b,a,c],1). (Falla pues c no está en el alfabeto.)
    false.
acepta([b,a],2). (Falla pues el estado 2 no tiene función de transición para b.)
    false.

Ejercicio 2.c) Verificamos la ruta que sigió el automata al recibir la cadena
               "baba".
ejercicio_2c(X).
    X = [((1, b), 2),  ((2, a), 3),  ((3, b), 2),  ((2, a), 3)].
