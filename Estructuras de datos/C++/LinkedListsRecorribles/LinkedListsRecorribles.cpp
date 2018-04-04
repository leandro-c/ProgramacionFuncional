#include <stdio.h>
#include <malloc.h>

#include "..\Prelude\Prelude.h"
#include "LinkedListsRecorribles.h"

/*************************************/
/* Implementacion de las operaciones */
/*                                   */
/* OBS:                              */
/*   mkCons, etc. reciben una        */
/*   referencia para enfatizar que   */
/*   que se trata de operaciones     */
/*   destructivas                    */
/*************************************/

List Nil()
/*
   PROPOSITO: construye la lista vacia
   PRECOND: ninguna, es una operacion total
*/
{
  lHeader* newList = new lHeader;
  newList->firstElem   = NULL;
  newList->lastElem    = NULL;
  newList->currentElem = NULL;
  newList->currentId   = 0;
  newList->nextId      = 1;
  return newList;
}

void mkCons(ELEM_TYPE x, List& xs)
/*
   PROPOSITO: agrega el elemento x adelante de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES:
    * modifica xs
    * anula el recorrido actual
*/
{
  lNode* newNode = new lNode;
  newNode->value = x;
  newNode->next  = xs->firstElem;
  xs->firstElem = newNode;
  if (xs->lastElem == NULL) xs->lastElem = newNode;
  xs->currentId = 0;
      // Privilegio el mkCons por sobre el recorrido actual!!!!
      //   (podría hacerse diferente y privilegiar el recorrido)
}

void mkSnoc(List & xs, ELEM_TYPE x)
/*
   PROPOSITO: agrega el elemento x atras de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES:
    * modifica xs
    * anula el recorrido actual
*/
{
  lNode* newNode = new lNode;
  newNode->value = x;
  newNode->next  = NULL;
  if (xs->lastElem == NULL)
        xs->firstElem = newNode;
  else  xs->lastElem->next = newNode;
  xs->lastElem = newNode;
  xs->currentId = 0;
      // Privilegio el mkSnoc por sobre el recorrido actual!!!!
      //   (podría hacerse diferente y privilegiar el recorrido)
}

bool isNil(List xs)
/*
   PROPOSITO: pregunta si es la lista vacia
   PRECOND: ninguna, es una operacion total
*/
{ return (xs->firstElem == NULL); }

ELEM_TYPE splitHead(List & xs)
/*
   PROPOSITO: devuelve el primer elemento de la lista, y modifica la lista para que no
              tenga mas primer elemento
   PRECOND: xs no es vacia
   OBSERVACIONES:
    * modifica xs.
    * libera memoria
*/
{
  lNode* tempNode = xs->firstElem;
  ELEM_TYPE temp = tempNode->value;
  xs->firstElem = tempNode->next;
  delete(tempNode);
  if (xs->firstElem == NULL) xs->lastElem = NULL;
  xs->currentId = 0;
  return temp;
}

/*************************************/
Handle IniciarRecorrido(List & xs)
{
    if (xs->currentId != 0) exit(1);
    xs->currentId = xs->nextId++;
        //xs->currentId = xs->nextId;
        //xs->nextId = xs->nextId + 1; Posincremento
    xs->currentElem = xs->firstElem;
    return (xs->currentId);
}

bool finRecorrido(List xs, Handle h)
{
    if (xs->currentId != h) exit(1);
    return (xs->currentElem == NULL);

}

ELEM_TYPE elementoActual(List xs, Handle h)
{
    if (xs->currentId != h) exit(1);
    return (xs->currentElem->value);
}

void PasarAlSiguiente(List & xs, Handle h)
{
    if (xs->currentId != h) exit(1);
    xs->currentElem = xs->currentElem->next;
}

void FinalizarRecorrido(List & xsref, Handle h)
{
    if (xsref->currentId != h) exit(1);
    xsref->currentId = 0;
}

/*************************************/
List copiar(List xs) {
   Handle h = IniciarRecorrido(xs);
   List newList = Nil();
   while (not finRecorrido(xs,h))
   {
      mkSnoc(newList, elementoActual(xs,h));
      PasarAlSiguiente(xs,h);
   }
   FinalizarRecorrido(xs,h);
   return newList;
}


/*************************************/
void printList(List xs)
/*
   PROPOSITO: imprime la lista
   PRECOND: ninguna, es una operacion total
*/
{
  if (isNil(xs))
   { printf("[]"); }
  else
   {
     Handle h = IniciarRecorrido(xs);
     printf("[ ");
     printElemType(elementoActual(xs,h));
     PasarAlSiguiente(xs,h);
     while (not finRecorrido(xs,h))
     {
       printf(", ");
       printElemType(elementoActual(xs,h));
       PasarAlSiguiente(xs,h);
     }
     printf(" ]");
     FinalizarRecorrido(xs,h);
   }
}

