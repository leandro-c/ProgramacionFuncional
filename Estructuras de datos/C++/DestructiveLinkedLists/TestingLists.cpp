#include <stdio.h>

#include "..\Prelude\Prelude.h"
#include "TestingLists.h"

void testingLists(ListInt xs)
{
  ListInt as = Nil();
  mkCons(16, as);
  mkCons(8, as);
  mkCons(4, as);
  mkCons(2, as);

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

int ilength(List xs)
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES: la lista tiene que estar correctamente armada
*/
{
  List current = copiar(xs);
  int len = 0;                // IniciarRecorrido()
  while (not isNil(current))  // while (not finRecorrido())
   {                          // {
     len++;                   //   ProcesarElemento()
     tkTail(current);         //   PasarAlSiguienteElemento()
   }                          // }
  return len;                 // FinRecorrido()
}

List ireverse(List xs)
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
*/
{
   List current = xs;
   List newList = Nil();
   while (not isNil(current))
   {
      mkCons(head(current), newList);
      current = current->next;
   }
   return newList;
}


ListInt isuccl(ListInt xs)
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
*/
{
   ListInt current = ireverse(xs);
   ListInt newList = Nil();
   while (not isNil(current))
   {
      mkCons(head(current)+1, newList);
      current = current->next;
   }
   return newList;
}

List iappend(List xs, List ys)
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES:
    * el reverse de xs en current es caro!
    * la memoria de current se desperdicia!!!!
*/
{
   List current = ireverse(xs);
   List finalList = copiar(ys);
   while (not isNil(current))
   {
      mkCons(head(current), finalList);
      current = current->next;
   }
   return finalList;
}

/************************************/
/* Algoritmos de listas, recursivos */
/************************************/
int length(List xs)
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
*/
{
  if (isNil(xs))
       return 0;                     // La lista vacia no tiene elementos
  else return length(xs->next) + 1;  // Cons agrega un elemento a tail(xs)

/* // Version larga, con un unico return
  int resultado;
  ELEM_TYPE y;
  List ys;
  if (isNil(current))
     resultado = 0;               // La lista vacia no tiene elementos
  else
   {
     y  = head(xs)                // Damos nombre al primer elemento
     ys = tail(xs)                // Damos nombre a los demas elementos
     resultado = length(ys) + 1;  // Cons agrega un elemento, y entonces se suma 1
   }
  return resultado;
*/
}

List reverse(List xs)
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
*/
{
  ListInt ys;
  if (isNil(xs))
      ys = Nil();
  else
    {
      ys = reverse(xs->next);
      mkSnoc(ys, head(xs));  // agrega el primer elemento al final del reverso del resto
    }
  return ys;
}

ListInt succl(ListInt xs)
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
*/
{
  ListInt ys;
  if (isNil(xs))
      ys = Nil();
  else
    {
      ys = succl(xs->next);
      mkCons(head(xs)+1, ys);
    }
  return ys;
}

List append(List xs, List ys)
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
*/
{
  ListInt zs;
  if (isNil(xs))
       zs = copiar(ys);
  else {
         zs = append(xs->next,ys);
         mkCons(head(xs), zs);
       }
  return zs;
}
