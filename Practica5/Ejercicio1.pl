/*******************************************************************************
*  Ejercicio1.pl - v1.0                                                        *
*                                                                              *
*                             Lógica Computacional                             *
*                     Facultad de Ciencias, UNAM, 2018-2                       *
*                        Todos los derechos reservados                         *
*                                                                              *
*                             Práctica 5: Prolog                               *
*                                                                              *
*  Objetivos y Anotaciones                                                     *
*     Mejorar el manejo el lenguaje de programación PROLOG.                    *
*                                                                              *
*  Profesor: Pilar Selene Linares Arévalo                                      *
*  Ayudante: Daniela Calderón Pérez                                            *
*  Ayud.Lab: Alejando Hernández Mora                                           *
*  Ayud.Lab: Luis Manuel Martínez Damaso                                       *
*                                                                              *
*******************************************************************************/

/***
  @descr <p> Programa para realizar el ejercicio 1 de la practica 5.</p>
    <ul>
      <li> Ver P05.pdf </li>
    </ul>
  @author Cortés López Jorge Francisco
  @author López Arias Victor Ulises
  @author Sainz Takata Izumi María
  @date 26/04/2018
*/

/**
  @form push(x, l, L)
    @constraints
      <ul>
        <li>x es un elemento</li>
        <li>l es una lista</li>
        <li>L es una lista</li>
      </ul>
   @descr Agregua un elemento al inicio de una lista.
*/
push(X, [], [X]).
push(X, [H|L], [X|[H|L]]).

/**
  @form pop([x|l], L)
    @constraints
      <ul>
        <li>x es un elemento</li>
        <li>l es una lista</li>
        <li>L es una lista</li>
      </ul>
   @descr Elimina el primer elemento de una lista.
*/
pop([], []).
pop([_|L], L).

/**
  @form append(l1, l2, L)
    @constraints
      <ul>
        <li>l1 es una lista</li>
        <li>l2 es una lista</li>
        <li>L es una lista</li>
      </ul>
   @descr Realiza la concatenación de dos listas.
*/
append([], L, L).
append(L, [], L).
append([H|T], L, [H|R]) :- append(T, L, R).

/**
  @form listSum(l, S)
    @constraints
      <ul>
        <li>l es una lista</li>
        <li>S es un entero</li>
      </ul>
   @descr Calcula la suma de los elementos en una lista.
*/
listSum([], S) :- S = 0.
listSum([H|T], S1) :-  integer(H), listSum(T, S2), S1 is H + S2.

/*******************************************************************************
*  Fin Ejercicio1.pl                                                           *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/