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
  @author López Arias Victor Ulises
  @author Sainz Takata Izumi María
  @author Cortés López Jorge Francisco
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
estado(1).
estado(2).
estado(3).

/**
  @form estado(x)
    @constraints
      <ul>
        <li>x es un estado</li>
      </ul>
   @descr Definimos el conjunto de estados iniciales del autómata.
*/
inicial(1).

/**
  @form estado(x)
    @constraints
      <ul>
        <li>x es un estado</li>
      </ul>
   @descr Definimos el conjunto de estados finales del autómata.
*/
final(3).

/**
  @form delta(q1, x, q2)
    @constraints
      <ul>
        <li>q1 es un estado</li>
        <li>x es un un elemento del alfabeto</li>
        <li>q2 es un estado</li>
      </ul>
   @descr Definimos el conjunto de funciones de transicion del autómata, 
          delta(q1, x, q2) <==> Desde el estado q1, al recibir x se llega a q2.
*/
delta(1, b, 2).
delta(2, a, 2).
delta(2, a, 3).
delta(3, b, 2).

/******************************************************************************/
/*                                   2.b)                                     */
/**
  @form acepta(C, Q)
    @constraints
      <ul>
        <li>C es una cadena</li>
        <li>Q es un estado</li>
      </ul>
   @descr Definimos una función que verifica si desde el estado Q se puede 
          llegar a u7n estado final leyendo la cadena (en forma de lista) C.
*/
acepta([], Q) :- final(Q). 

acepta([X|L], Q) :- 
    alfabeto(X), 
    estado(Q),
    delta(Q,X,R),
    acepta(L,R).

/******************************************************************************/
/*                                   2.c)                                     */
/**
  @form acepta_meta_aux(C, Q, S, M)
    @constraints
      <ul>
        <li>C es una cadena</li>
        <li>Q es un estado</li>
        <li>S es una lista de transiciones</li>
        <li>M es una lista de transiciones</li>
      </ul>
   @descr Auxiliar de @acepta_meta que rrealmente hace todo el trabajo.
*/
acepta_meta_aux([], Q, S, M) :- final(Q), reverse(S,M).

acepta_meta_aux([X|L], Q, S, M) :-
    alfabeto(X),
    delta(Q,X,R),
    acepta_meta_aux(L,R,[((Q,X),R)|S],M).

/**
  @form acepta_meta(C, M)
    @constraints
      <ul>
        <li>C es una cadena</li>
        <li>M es una lista de transiciones</li>
      </ul>
   @descr Definimos una función que nos regresa una lista de transiciones delta
          M cuando el autómata acepta la cadena C.
*/
acepta_meta(C, M) :-
    inicial(Q),
    acepta_meta_aux(C,Q,[],M).

/**
  @form ejercicio_2c(M)
    @constraints
      <ul>
        <li>M es una lista de transiciones</li>
      </ul>
   @descr Predicado que verifica si la cadena "baba" (en forma de lista de 
          caracteres) es aceptada por el autómata y guarda en M la meta que 
          siguió.
*/
ejercicio_2c(M) :- acepta_meta([b,a,b,a],M).

/*******************************************************************************
*  Fin Ejercicio2.pl                                                           *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/