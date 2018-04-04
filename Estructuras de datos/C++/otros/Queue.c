#include <stdio.h>
#include <malloc.h>

#include "Prelude.h"
#include "Queue.h"

/*************************************/
/* Implementacion de las operaciones */
/*************************************/

Queue emptyQueue()
/*
   PROPOSITO: construye la cola vacia
   PRECOND: ninguna, es una operacion total
*/
{ 
  Queue tope = malloc(sizeof(struct qTop));
  tope->first = NULL;
  tope->last  = NULL;
  return tope; 
}


void encolar(QUEUE_ELEM_TYPE x, Queue q)
/*
   PROPOSITO: agrega el elemento x al fondo de la cola q
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: modifica q! Es una operación destructiva!
*/
{
  // Se pide memoria
  struct qNode* newNode = malloc(sizeof(struct qNode));
  // Se asignan los elementos
  newNode->value = x;
  newNode->back  = NULL; 

  // Se modifican el ultimo, y si corresponde, el primero
  if (q->first == NULL)  
      q->first = newNode;      // q->last == NULL, por invariante
  else
      q->last->back = newNode; // q->last != NULL, por invariante
  q->last = newNode;
} 

Bool isEmptyQueue(Queue q)
/*
   PROPOSITO: pregunta si es la cola vacia
   PRECOND: ninguna, es una operacion total
   OBSERVACION: alcanza con preguntar si first es NULL, por invariante
*/
{ return (q->first == NULL); }

QUEUE_ELEM_TYPE elQueSigue(Queue q)
/*
   PROPOSITO: retorna el primero de la cola
   PRECOND: q no es vacia
*/
{ return (q->first->value); }

void descolar(Queue q)
/*
   PROPOSITO: elimina el primero de la cola
   PRECOND: q no es vacia
   OBSERVACIONES: modifica q y destruye el elemento descolado! Es una operación destructiva
*/
{ 
  struct qNode* temp = q->first;
  if (temp != NULL)
    {
      q->first = q->first->back;
      if (q->first == NULL) q->last == NULL;
      free(temp);
    }
  else
    BOOM("No se puede descolar una cola vacia!");
}

void printQueue(Queue q)
/*
   PROPOSITO: imprime la cola
   PRECOND: ninguna, es una operacion total
*/
{
  if (isEmptyQueue(q))
   { printf("<=<"); }
  else 
   {
     struct qNode* qq = q->first;
     printf("<=");
     printQueueElemType(qq->value);
     qq = qq->back;
     while (not (qq==NULL))
     {
       printf("=");
       printQueueElemType(qq->value);
       qq = qq->back;
     }
     printf("=<");
   }
}

