#include <stdio.h>
#include <malloc.h>

#include "Prelude.h"
#include "Stack.h"

/*************************************/
/* Implementacion de las operaciones */
/*************************************/

Stack emptyStack()
/*
   PROPOSITO: construye la pila vacia
   PRECOND: ninguna, es una operacion total
*/
{ return NULL; }


Stack push(STACK_ELEM_TYPE x, Stack p)
/*
   PROPOSITO: agrega el elemento x al tope de la pila p
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: no modifica xs, pero la comparte
*/
{
  // Se pide memoria
  struct sNode* newNode = malloc(sizeof(struct sNode));
  // Se asignan los elementos
  newNode->value = x;
  newNode->rest  = p; // Stack es (struct sNode*)
  // Se retorna la lista recien construida
  return (newNode);
} 

Bool isEmptyStack(Stack p)
/*
   PROPOSITO: pregunta si es la pila vacia
   PRECOND: ninguna, es una operacion total
*/
{ return (p == NULL); }

STACK_ELEM_TYPE top(Stack p)
/*
   PROPOSITO: retorna el tope de la pila
   PRECOND: p no es vacia
*/
{ return (p->value); }

Stack pop(Stack p)
/*
   PROPOSITO: elimina el tope de la pila
   PRECOND: p no es vacia
*/
{ return (p->rest); }

void printStack(Stack p)
/*
   PROPOSITO: imprime la pila
   PRECOND: ninguna, es una operacion total
*/
{
  if (isEmptyStack(p))
   { printf("<>~|"); }
  else 
   {
     Stack pp = p;
     printf("<>~");
     printStackElemType(top(pp));
     pp = pop(pp);
     while (not isEmptyStack(pp))
     {
       printf("~");
       printStackElemType(top(pp));
       pp = pop(pp);
     }
     printf("~|");
   }
}

