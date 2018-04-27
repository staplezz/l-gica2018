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
  @form ejercicio_1a(X,Y)
    @constraints
      <ul>
        <li>X es un booleano.</li>
        <li>Y es un booleano.</li>
      </ul>
   @descr Simula el digrama descrito en la práctica.
*/
ejercicio_1a(X,Y) :- and(not(and(not(X),Y)),Y).

/**
  @form ejercicio_1b(X,Y,Z)
    @constraints
      <ul>
        <li>X es un booleano.</li>
        <li>Y es un booleano.</li>
        <li>Z es un booleano.</li>
      </ul>
   @descr Simula el digrama descrito en la práctica.
*/
ejercicio_1b(X,Y,Z) :- and(not(and(not(X),X)),and(Y,Z)).

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
  @form ejercicio_(X,Y)
    @constraints
      <ul>
        <li>X es </li>
        <li>Y es </li>
      </ul>
   @descr ...
*/

/*******************************************************************************
*  Fin Practica4.pl                                                            *
*                                                                              *
*  Cortés López Jorge Francisco  314330981   kokofrank@ciencias.unam.mx        *
*  López Arias Víctor Ulises     310173335   ulises.lopez@ciencias.unam.mx     *
*  Sainz Takata Izumi María      314245195   sainz@ciencias.unam.mx            *
*                                                                              *
*******************************************************************************/