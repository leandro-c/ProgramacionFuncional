#include <stdio.h>

#include "..\Prelude\Prelude.h"
#include "TestingLists.h"

void testingLists(ListInt xs)
{
  printf("\n");
  printf("xs             = "); printList(xs); printf("\n");
  printf("ilength(xs)    = %d\n", ilength(xs));
  printf("ireverse(xs)   = "); printList(ireverse(xs)); printf("\n");
  printf("isuccl(xs)     = "); printList(isuccl(xs)); printf("\n");
  printf("\n");
  printf("length(xs)     = %d\n", length(xs));
  printf("reverse(xs)    = "); printList(reverse(xs)); printf("\n");
  printf("succl(xs)      = "); printList(succl(xs)); printf("\n");
  printf("\n");

  ListInt ys = iappend(xs, isuccl(xs));
  printf("ys=append(xs,succl(xs)) = "); printList(ys); printf("\n");
  printf("ilength(ys)             = %d\n", ilength(ys));
  printf("ireverse(ys)            = "); printList(ireverse(ys)); printf("\n");
  printf("isuccl(ys)              = "); printList(isuccl(ys)); printf("\n");
  printf("\n");
  printf("length(ys)              = %d\n", length(ys));
  printf("reverse(ys)             = "); printList(reverse(ys)); printf("\n");
  printf("succl(ys)               = "); printList(succl(ys)); printf("\n");
  printf("\n");
}

/************************************/
/* Algoritmos de listas, iterativos */
/************************************/

int ilength(List xs) {
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * utiliza las operaciones de recorrido
*/

  Handle h = IniciarRecorrido(xs);
  int len = 0;                    // IniciarRecorrido()
  while (not finRecorrido(xs,h))  // while (not finRecorrido())
   {                              // {
     len++;                       //   ProcesarElemento()
     PasarAlSiguiente(xs,h);      //   PasarAlSiguienteElemento()
   }                              // }
  FinalizarRecorrido(xs,h);       // FinalizarRecorrido()
  return len;
}

List ireverse(List xs) {
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * utiliza las operaciones de recorrido
    * hace un recorrido directo usando Cons, para obtener reverse
*/

   Handle h = IniciarRecorrido(xs);
   List newList = Nil();
   while (not finRecorrido(xs,h))
   {
      mkCons(elementoActual(xs,h), newList);
      PasarAlSiguiente(xs,h);
   }
   FinalizarRecorrido(xs,h);
   return newList;
}

ListInt isuccl(ListInt xs) {
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * utiliza las operaciones de recorrido
    * hace un recorrido directo usando Snoc, para conservar el orden
*/

   Handle h = IniciarRecorrido(xs);
   ListInt newList = Nil();
   while (not finRecorrido(xs,h))
   {
      mkSnoc(newList, elementoActual(xs,h)+1);
      PasarAlSiguiente(xs,h);
   }
   FinalizarRecorrido(xs,h);
   return newList;
}

List iappend(List xs, List ys) {
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * usa las operaciones de recorrido
    * usa un recorrido directo sobre la SEGUNDA lista, agregando con Snoc
       para conservar el orden
    * NO ES POSIBLE hacer un recorrido directo sobre la primera lista, y
       acá un recorrido reverso es caro
*/

   Handle h = IniciarRecorrido(ys);
   List finalList = copiar(xs);
   while (not finRecorrido(ys,h))
   {
      mkSnoc(finalList, elementoActual(ys,h));
      PasarAlSiguiente(ys,h);
   }
   FinalizarRecorrido(ys,h);
   return finalList;
}

/************************************/
/* Algoritmos de listas, recursivos */
/************************************/
int lengthRep(List& xsref, Handle h);

int length(List xs) {
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * usa las operaciones de recorrido
    * debe utilizar un auxiliar recursivo para hacer la recursion,
       pues Iniciar y Finalizar no son parte recursiva!
*/
  Handle h = IniciarRecorrido(xs);
  int lxs = lengthRep(xs, h);
  FinalizarRecorrido(xs,h);
  return lxs;
}

int lengthRep(List& xs, Handle h) {
/*
    PROPOSITO: calcular la longitud recursivamente sobre una
               lista destructiva, pero solo de su porcion no
               recorrida
    PRECOND: hay un recorrido iniciado con handle h
    OBSERVACIONES:
    * NO modifica los elementos de xs
    * como es recursiva no puede iniciar ni finalizar el recorrido!!!!
*/
  if (finRecorrido(xs, h))
      return 0;                     // La lista vacia no tiene elementos
  else {
      PasarAlSiguiente(xs,h);
      return lengthRep(xs,h) + 1;  // Cons agrega un elemento a tail(xs)
  }
}

void revRep(List& xs, Handle h, List& result);

List reverse(List xs) {
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
*/
  Handle h = IniciarRecorrido(xs);
  List ys = Nil();
  revRep(xs, h, ys);
  FinalizarRecorrido(xs,h);
  return ys;
}

void revRep(List& xs, Handle h, List& result) {
/*
    PROPOSITO: agrega los elementos por recorrer de
               xsref a resultRef, en orden reverso
    PRECOND: hay un recorrido iniciado con handle h
    OBSERVACIONES:
    * NO modifica los elementos de xs
    * en el caso base, el resultado es resultRef, por lo que no se hace nada
    * observar que el elemento actual x debe obtenerse si o si ANTES de pasar al siguiente
    * una alternativa seria deshacer la operacion de pasar al siguiente (backtracking!!)
*/
  if (not finRecorrido(xs,h)) {
     int x = elementoActual(xs, h);
     PasarAlSiguiente(xs, h);
     revRep(xs, h, result);
     mkSnoc(result, x);
  }
}

void succlRep(List& xs, Handle h, List& result);
ListInt succl(ListInt xs) {
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
*/
  Handle h = IniciarRecorrido(xs);
  List ys = Nil();
  succlRep(xs, h, ys);
  FinalizarRecorrido(xs,h);
  return ys;
}

void succlRep(List& xs, Handle h, List& result) {
/*
    PROPOSITO: agrega los elementos por recorrer de xsref modificados a resultRef,
               en orden directo
    PRECOND: hay un recorrido iniciado con handle h
    OBSERVACIONES:
    * NO modifica los elementos de xs
    * en el caso base, el resultado es resultRef, por lo que no se hace nada
    * observar que el elemento actual x debe obtenerse si o si ANTES de pasar al siguiente
    * una alternativa seria deshacer la operacion de pasar al siguiente (backtracking!!)
*/
  if (not finRecorrido(xs, h)) {
     int x = elementoActual(xs, h);
     PasarAlSiguiente(xs, h);
     succlRep(xs, h, result);
     mkCons(x+1,result);
  }
}

void appRep(List& xsref, Handle h, List& resultRef);
List append(List xs, List ys) {
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
*/
  Handle h = IniciarRecorrido(xs);
  List zs = copiar(ys);
  appRep(xs, h, zs);
  FinalizarRecorrido(xs, h);
  return zs;
}

void appRep(List& xs, Handle h, List& result) {
/*
    PROPOSITO: agrega los elementos por recorrer de xsref a resultRef,
               en orden directo (obteniendo append)
    PRECOND: hay un recorrido iniciado con handle h
    OBSERVACIONES:
    * NO modifica los elementos de xs
    * en el caso base, el resultado es resultRef, por lo que no se hace nada
    * observar que el elemento actual x debe obtenerse si o si ANTES de pasar al siguiente
    * una alternativa seria deshacer la operacion de pasar al siguiente (backtracking!!)
*/
  if (not finRecorrido(xs, h)) {
     int x = elementoActual(xs, h);
     PasarAlSiguiente(xs, h);
     appRep(xs, h, result);
     mkCons(x, result);
  }
}
