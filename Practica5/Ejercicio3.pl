/*******************************************************************************
*  Ejercicio3.pl - v1.0                                                        *
*                                                                              *
*                             Lógica Computacional                             *
*                     Facultad de Ciencias, UNAM, 2018-2                       *
*                        Todos los derechos reservados                         *
*                                                                              *
*                             Práctica 5: Prolog                               *
*                                                                              *
*  Objetivos y Anotaciones                                                     *
*     Mejorar el manejo del lenguaje de programación PROLOG.                   *
*                                                                              *
*  Profesor: Pilar Selene Linares Arévalo                                      *
*  Ayudante: Daniela Calderón Pérez                                            *
*  Ayud.Lab: Alejando Hernández Mora                                           *
*  Ayud.Lab: Luis Manuel Martínez Damaso                                       *
*                                                                              *
*******************************************************************************/

/***
  @descr <p> Programa para realizar el ejercicio 3 de la practica 5.</p>
    <ul>
      <li> Ver P05.pdf </li>
    </ul>
  @author López Arias Victor Ulises
  @author Sainz Takata Izumi María
  @author Cortés López Jorge Francisco
  @date 11/05/2018
*/

/******************************************************************************/
/*                                   3.a)                                     */

/**
  @form pila(x)
    @constraints
      <ul>
        <li>x es una pila</li>
      </ul>
   @descr Definimos las pilas.
*/
pila([]).
pila([_|P]) :- pila(P).

/**
  @form state(X, Y, Z)
    @constraints
      <ul>
        <li>X es una pila</li>
        <li>Y es una pila</li>
        <li>Z es una pila</li>
      </ul>
   @descr Definimos los estados como una terna de pilas en las que el cubo más a
          la derecha de cada una está colocado en el piso.
*/
state(X, Y, Z) :- 
    pila(X),
    pila(Y),
    pila(Z).

/******************************************************************************/
/*                                   3.b)                                     */

/**
  @form move(S1, S2)
    @constraints
      <ul>
        <li>S1 es un estado</li>
        <li>S2 es un estado</li>
      </ul>
   @descr Definimos los movimientos válidos de estado a estado, en los que sólo
          se puede cambiar el cubo de más arriba (al ser "listas", más a la iz-
          quierda) en una pila hacia otra.
*/
move(state([Cx|X],Y,_), state(X,[Cx|Y],_)).
move(state(X,[Cy|Y],_), state([Cy|X],Y,_)).

move(state(_,[Cy|Y],Z), state(_,Y,[Cy|Z])).
move(state(_,Y,[Cz|Z]), state(_,[Cz|Y],Z)).

move(state([Cx|X],_,Z), state(X,_,[Cx|Z])).
move(state(X,_,[Cz|Z]), state([Cz|X],_,Z)).


/******************************************************************************/
/*                                   3.c)                                     */

/**
  @form en(X,L)
    @constraints
      <ul>
        <li>X es un elemento</li>
        <li>L es una lista</li>
      </ul>
   @descr Se cumple si X elemento de L.
*/
en(X,[Y|L]) :- X = Y; en(X,L). 

/**
  @form busca_path(S1, S2,L1,S)
    @constraints
      <ul>
        <li>S1 es un estado</li>
        <li>S2 es un estado</li>
        <li>L1 es una lista</li>
        <li>S  es una lista</li>
      </ul>
   @descr función auxiliar de @path/3 que realmente hace todo el trabajo.
*/
busca_path(void,void,L1,S) :- reverse(L1,S). 
busca_path(S1, S2, L1, S).

/**
  @form busca_path(S1, S2, S)
    @constraints
      <ul>
        <li>S1 es un estado</li>
        <li>S2 es un estado</li>
        <li>S  es una lista</li>
      </ul>
   @descr Definimos a S como el la serie de pasos que se deben de tomar para 
          pasar del estado S1 al estado S2.
*/
path(state(X1,Y1,Z1), state(X2,Y2,Z2), S) :-
    busca_path(state(X1,Y1,Z1), state(X2,Y2,Z2), [], S).

/*******************************************************************************
*  Fin Ejercicio3.pl                                                           *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/