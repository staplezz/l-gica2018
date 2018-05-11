/*******************************************************************************
*  Ejercicio2.pl - v1.0                                                        *
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
  @descr <p> Programa para realizar el ejercicio 2 de la practica 5.</p>
    <ul>
      <li> Ver P05.pdf </li>
    </ul>
  @author Cortés López Jorge Francisco
  @author López Arias Victor Ulises
  @author Sainz Takata Izumi María
  @date 11/05/2018
*/

/******************************************************************************/
/*                                   2.a)                                     */
/**
  @form alfabeto(x)
    @constraints
      <ul>
        <li>x es una literal</li>
      </ul>
   @descr Definimos el conjunto de elementos del alfabeto del autómata.
*/
alfabeto(a).
alfabeto(b).

/**
  @form estado(x)
    @constraints
      <ul>
        <li>x es un estado</li>
      </ul>
   @descr Definimos el conjunto de estados del autómata.
*/
estado(q1).
estado(q2).
estado(q3).

/**
  @form estado(x)
    @constraints
      <ul>
        <li>x es un estado</li>
      </ul>
   @descr Definimos el conjunto de estados iniciales del autómata.
*/
inicial(q1).

/**
  @form estado(x)
    @constraints
      <ul>
        <li>x es un estado</li>
      </ul>
   @descr Definimos el conjunto de estados finales del autómata.
*/
final(q3).

/**
  @form delta(q1, x, q2)
    @constraints
      <ul>
        <li>q1 es un estado</li>
        <li>x es un un elemento del alfabeto</li>
        <li>q2 es un estado</li>
      </ul>
   @descr Definimos el conjunto de funciones de transicion del autómata, 
          delta(q1, x, q2) <=> En el estado q1, al recibir x se llega a q2.
*/
delta(q1, b, q2).
delta(q2, a, q2).
delta(q2, a, q3).
delta(q3, b, q2).

/******************************************************************************/
/*                                   2.b)                                     */
acepta([], Q) :- final(Q). 

acepta([X|L], Q) :- 
    alfabeto(X), 
    estado(Q),
    delta(Q,X,R),
    acepta(L,R).

/******************************************************************************/
/*                                   2.c)                                     */
acepta_meta(L, M) :-
    inicial(Q),
    acepta_meta_aux(L,Q,[],M).

acepta_meta_aux([], Q, S, M) :- final(Q), reverse(S,M).

acepta_meta_aux([X|L], Q, S, M) :-
    alfabeto(X),
    delta(Q,X,R),
    acepta_meta_aux(L,R,[((Q,X),R)|S],M).

ejercicio_2c(X = [b,a,b,a],M) :- acepta_meta(X,M), imprime(M).

/*******************************************************************************
*  Fin Ejercicio2.pl                                                           *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/