Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx
López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx
Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx


Pruebas recomendadas: 

Ejercicio 1.a)
push(x, [], X).
    X = [x].
push(x, [y,z], X).
    X = [x, y, z].

Ejercicio 1.b)
pop([],X).
    X = [].
pop([x],X).
    X = [].
pop([x,y,z],X).
    X = [y, z].

Ejercicio 1.c)
append([],[x,y,z],X).
X = [x, y, z].
append([a,b,c],[],X).
X = [a, b, c].
append([a,b,c],[x,y,z],X).
X = [a, b, c, x, y, z].

Ejercicio 1.d)
listSum([1],X).
    X = 1.
listSum([],X).
    X = 0.
listSum([1,2,3,4],X).
    X = 10 .

