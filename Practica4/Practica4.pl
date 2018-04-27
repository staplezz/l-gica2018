/*******************************************************************************
*  Practica4.pl - v1.0                                                         *
*                                                                              *
*                             Lógica Computacional                             *
*                     Facultad de Ciencias, UNAM, 2018-2                       *
*                        Todos los derechos reservados                         *
*                                                                              *
*                             Práctica 4: Prolog                               *
*                                                                              *
*  Objetivos y Anotaciones                                                     *
*     Que el alumno utilice3 el lenguaje de programació PROLOG.                *
*                                                                              *
*  Profesor: Pilar Selene Linares Arévalo                                      *
*  Ayudante: Daniela Calderón Pérez                                            *
*  Ayud.Lab: Alejando Hernández Mora                                           *
*  Ayud.Lab: Luis Manuel Martínez Damaso                                       *
*                                                                              *
*******************************************************************************/

/***
  @descr <p> Programa para realizar lo ejercicios de la prática 4. </p>
    <ul>
      <li> Ver P04.pdf </li>
    </ul>
  @author Cortés López Jorge Francisco
  @author López Arias Victor Ulises
  @author Sainz Takata Izumi María
  @date 26/04/2018
*/

/**
  @form and(X,Y)
    @constraints
      <ul>
        <li>X es true o false</li>
        <li>Y es true o false</li>
      </ul>
   @descr Simula la compuerta lógica AND.
*/
and(true,true).

/**
  @form not(X)
    @constraints
      <ul>
        <li>X es true o false</li>
      </ul>
   @descr Simula la compuerta lógica NOT.
*/
not(false).

/**
  @form circuito_1a(X,Y)
    @constraints
      <ul>
        <li>X es un booleano.</li>
        <li>Y es un booleano.</li>
      </ul>
   @descr Simula el digrama descrito en la práctica.
*/
circuito_1a(X,Y) :- and(X,Y).

/**
  @form circuito_1b(X,Y,Z)
    @constraints
      <ul>
        <li>X es un booleano.</li>
        <li>Y es un booleano.</li>
        <li>Z es un booleano.</li>
      </ul>
   @descr Simula el digrama descrito en la práctica.
*/
circuito_1b(_,Y,Z) :- and(Y,Z).

/**
  @form bt(X)
    @constraints
      <ul>
        <li> @void Es un árbol. </li>
        <li>Un nodo que contiene a un entero y dos árboles es un árbol. </li>
      </ul>
   @descr Representamos el predicado ser árbol binario.
*/
bt(void).
bt(node(A,T1,T2)):- integer(A),bt(T1),bt(T2).

/**
  @form elem(A,T)
    @constraints
      <ul>
        <li>A es un entero.</li>
        <li>T es un árbol. </li>
      </ul>
   @descr Se cumple si A es elemento de T.
*/
elem(A,bt(node(B,T1,T2))) :- A = B; elem(A,T1); elem(A,T2).

/**
  @form maxtree(A,T)
    @constraints
      <ul>
        <li>A es un entero.</li>
        <li>T es un árbol. </li>
      </ul>
   @descr Se cumple si A es mayor que todos los elementos de T.
*/
maxtree(A,bt(node(B,T1,T2))) :- A>B, maxtree(A,T1), maxtree(A,T2).
maxtree(_,void).

/**
  @form mintree(A,T)
    @constraints
      <ul>
        <li>A es un entero.</li>
        <li>T es un árbol. </li>
      </ul>
   @descr Se cumple si A es menor que todos los elementos de T.
*/
mintree(A,bt(node(B,T1,T2))) :- A<B, mintree(A,T1), mintree(A,T2).
mintree(_,void).

/**
  @form conecta(X,Y)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>Y es un vertice.</li>
      </ul>
   @descr Conectamos vertices.
*/
conecta(a,b).
conecta(b,c).
conecta(b,e).
conecta(c,d).
conecta(d,e).
conecta(e,f).
conecta(e,g).

/**
  @form vecino(X,Y)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>Y es un vertice.</li>
      </ul>
   @descr Se cumple si X y Y son conexos.
*/
vecino(X,Y) :- conecta(X,Y); conecta(Y,X).

/**
  @form entrada(X)
    @constraints
      <ul>
        <li>X es un vertice</li>
      </ul>
   @descr Secumple si X es la entrada a la casa. Lo declaramos.
*/
entrada(a).

/**
  @form telefono(X)
    @constraints
      <ul>
        <li>X es un vertice</li>
      </ul>
   @descr Se cumple si X tiene el telefono de la casa. Lo declaramos.
*/
telefono(g).

/**
  @form buscaCamino(X,Y,R)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>Y es un vertice.</li>
        <li>R es una lista de vertices.</li>
      </ul>
   @descr Busca un camino desde el vertice X hasta el vertice Y y lo guarda en 
          R.
*/
buscaCamino(X,Y) :- buscaCaminoSin(X,Y,[],R).

/**
  @form buscaCaminoSin(X,Y,L,R)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>Y es un vertice.</li>
        <li>L es una lista de vertices.</li>
        <li>R es una lista de vertices.</li>
      </ul>
   @descr Busca un camino desde el vertice X hasta el vertice Y sin pasar por 
          ningún vertice que esté en L y la guarda en R.
*/
buscaCaminoSin(X,Y,L,R):-
  V = [],
  camino(X,Y,V,L,R).

/**
  @form camino(X,Y,V,L)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>Y es un vertice.</li>
        <li>V es una lista de vertices.</li>
        <li>L es una lista de vertices.</li>
        <li>R es una lista de vertices.</li>
      </ul>
   @descr Busca un camino desde el vertice X hasta el vertice Y sin pasar por 
          ningún vertice que esté en L y va guardandolo en R.
*/
camino(X,Y,V,L,R):-
        en(X,V),
        !;
        T = [X|V],
        siguiente(T,vecinos(X),L,Z),
        Z = Y,
        R = [Z|T],
        !;
        camino(Z,Y,T,L,R).

/**
  @form en(X,L)
    @constraints
      <ul>
        <li>X es un vertice.</li>
        <li>L es una lista de vertices.</li>
      </ul>
   @descr Se cumple si X elemento de L.
*/
en(X,[Y|L]) :- X = Y; en(X,L). 

/*******************************************************************************
*  Fin Practica4.pl                                                            *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/



/**
  @form ejercicio_(X,Y)
    @constraints
      <ul>
        <li>X es </li>
        <li>Y es </li>
      </ul>
   @descr ...
*/