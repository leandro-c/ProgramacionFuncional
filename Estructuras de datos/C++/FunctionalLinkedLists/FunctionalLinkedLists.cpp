#include <stdio.h>
#include <malloc.h>

#include "..\Prelude\Prelude.h"
#include "FunctionalLinkedLists.h"

struct lNode{
   ELEM_TYPE     value;
   lNode*        next;
};

/*************************************/
/* Implementacion de las operaciones */
/*************************************/

List Nil()
/*
   PROPOSITO: construye la lista vacia
   PRECOND: ninguna, es una operacion total
*/
{ return NULL; }


List Cons(ELEM_TYPE x, List xs)
/*
   PROPOSITO: agrega el elemento x adelante de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: no modifica xs, pero la comparte
*/
{
  // Se pide memoria
  lNode* newNode = new lNode;
  // Se asignan los elementos
  newNode->value = x;
  newNode->next  = xs; // List es (struct lNode*)
  // Se retorna la lista recien construida
  return (newNode);
}

List Snoc(List xs, ELEM_TYPE x)
/*
   PROPOSITO: agrega el elemento x atras de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: NO modifica xs como efecto lateral, pero la copia!!
*/
{
  List retorno;
  List current;
  lNode* lastSeen;
  lNode* node;

  // Se pide memoria
  lNode* newNode = new lNode;
  // Se asignan los elementos

  newNode->value = x;
  newNode->next  = NULL;
.
  // Se copia la lista hasta llegar al ultimo elemento, y
  // se engancha el newNode
  if (isNil(xs))
    retorno = newNode;
  else
    {
     /*
        retorno = copiarLista(xs);
        current = retorno;
        while (not (current->next == NULL))
          current = current->next;

        current->next = newNode;
     */
        // El primer elemento de xs existe, y es el primero de la nueva lista
        retorno = new lNode;
        retorno->value = xs->value;
        lastSeen = retorno;
        //lastSeen->next queda pendiente para que lo inicialice el siguiente!!
        current = xs->next;
        while(not isNil(current))
          {
            node = new lNode;
            lastSeen->next = node;
            node->value = current->value;

            // Al pasar al siguiente, actualizo lastSeen
            // para que su siguiente se inicialice bien después
            lastSeen = node;
            current = current->next;
          }
        lastSeen->next = newNode;
    }

  // Se retorna la lista recien construida
  return (retorno);
}

bool isNil(List xs)
/*
   PROPOSITO: pregunta si es la lista vacia
   PRECOND: ninguna, es una operacion total
*/
{ return (xs == NULL); }

ELEM_TYPE head(List xs)
/*
   PROPOSITO: devuelve el primer elemento de la lista
   PRECOND: xs no es vacia
*/
{ return (xs->value); }

List tail(List xs)
/*
   PROPOSITO: devuelve la lista sin el primer elemento
   PRECOND: xs no es vacia
*/
{ return (xs->next); }

bool matchCons(List xs, ELEM_TYPE & yref, List & ysref)
/*
   PROPOSITO: hace el pattern matching de xs con (y:ys)
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: y e ys no tendran valor si
                  matchCons devuelve False
*/
{
    if (xs != NULL) {
        yref = xs->value;
        ysref = xs->next;
    }
    return (xs!=NULL);
}

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
     List ys = xs;
     printf("[ ");
     printElemType(head(ys));
     ys = tail(ys);
     while (not isNil(ys))
     {
       printf(", ");
       printElemType(head(ys));
       ys = tail(ys);
     }
     printf(" ]");
   }
}

