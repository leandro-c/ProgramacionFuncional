#include <stdio.h>
#include <malloc.h>

#include "..\Prelude\Prelude.h"
#include "AppendableListsRecorribles.h"

void consLink(lNode* x, List& xsref);
void snocLink(List& xsref, lNode* x);

/*************************************/
/* Implementacion de las operaciones */
/*                                   */
/* OBS:                              */
/*   mkCons, etc. reciben una        */
/*   referencia para enfatizar que   */
/*   que se trata de operaciones     */
/*   destructivas                    */
/*************************************/

List Nil() {
/*
   PROPOSITO: construye la lista vacia
   PRECOND: ninguna, es una operacion total
*/
  lHeader* newList = new lHeader;
  newList->firstElem   = NULL;
  newList->lastElem    = NULL;
  newList->currentElem = NULL;
  newList->currentId   = 0;
  newList->nextId      = 1;
  return newList;
}

void mkCons(ELEM_TYPE x, List& xs) {
/*
   PROPOSITO: agrega el elemento x adelante de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES:
    * modifica xs
    * anula el recorrido actual
*/
  lNode* newNode = new lNode;
  newNode->value = x;
  consLink(newNode, xs);
}

void consLink(lNode* newNode, List& xsref) {
/*
   PROPOSITO: engancha el nodo dado adelante de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES:
    * modifica xs Y newNode
    * anula el recorrido actual
*/
  newNode->next  = xsref->firstElem;
  xsref->firstElem = newNode;
  if (xsref->lastElem == NULL) xsref->lastElem = newNode;
  xsref->currentId = 0;
}

void mkSnoc(List& xs, ELEM_TYPE x)
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
  snocLink(xs, newNode);
}

void snocLink(List& xsref, lNode* newNode) {
  if (xsref->lastElem == NULL)
        xsref->firstElem = newNode;
  else  xsref->lastElem->next = newNode;
  xsref->lastElem = newNode;
  xsref->currentId = 0;
}

bool isNil(List xs)
/*
   PROPOSITO: pregunta si es la lista vacia
   PRECOND: ninguna, es una operacion total
*/
{ return (xs->firstElem == NULL); }

ELEM_TYPE splitHead(List& xsref)
/*
   PROPOSITO: devuelve el primer elemento de la lista, y modifica la lista para que no
              tenga mas primer elemento
   PRECOND: xs no es vacia
   OBSERVACIONES:
    * modifica xs.
    * libera memoria
*/
{
  lNode* tempNode = xsref->firstElem;
  ELEM_TYPE temp = tempNode->value;
  xsref->firstElem = tempNode->next;
  free(tempNode);
  if (xsref->firstElem == NULL) xsref->lastElem = NULL;
  xsref->currentId = 0;
  return temp;
}

/*************************************/
Handle IniciarRecorrido(List& xsref)
{
    xsref->currentId = xsref->nextId++;
    xsref->currentElem = xsref->firstElem;
    return (xsref->currentId);
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

void PasarAlSiguiente(List& xsref, Handle h)
{
    if (xsref->currentId != h) exit(1);
    xsref->currentElem = xsref->currentElem->next;
}

void FinalizarRecorrido(List& xsref, Handle h)
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
void mkInsert(int x, List& xsref) {
/*
    PROPOSITO: inserta x de manera ordenada en la lista
    PRECOND: la lista xs esta ordenada
    OBSERVACIONES: es una operacion destructiva
*/
  struct lNode* current;

  // Se prepara el nodo para x
  lNode* newNode = new lNode;
  newNode->value = x;
//  newNode->next  = Nil();

  // Se calcula el punto de insercion
  if (isNil(xsref) || x <= xsref->firstElem->value)
     consLink(newNode, xsref);
  else
   {
     current = xsref->firstElem;
     while (not (current->next == NULL) && x > current->next->value)
       current = current->next;

     // Es casi consLink, pero con otro espacio de memoria
     // Capturar esa similitud seria complejo y poco conveniente
     newNode->next = current->next;
     current->next = newNode;
     if (xsref->lastElem == current) xsref->lastElem = newNode;
     xsref->currentId = 0;
   }
}

void mkAppend(List& xs, List ys){
/*
    PROPOSITO: agrega los elementos de ys a xs como un append, pero destruye ys
    OBSERVACIONES:
    * modifica xs
    * NO destruye ys, sino una copia
*/
    List ysCopy = copiar(ys);
    mkDump(xs,ysCopy);
}

void mkDump(List& xs, List& ys) {
/*
    PROPOSITO: agrega los elementos de ys a xs como un append, pero destruye ys
    OBSERVACIONES:
    * modifica xs
    * destruye ys
*/
    if (not isNil(ys)) {
        snocLink(xs, ys->firstElem);
        // OJO con el ultimo elemento de xs!
        xs->lastElem = ys->lastElem;
        // Corrige el ultimo elemento de xs
    }
    delete(ys);
}

/*************************************/
void printList(List xs)
/*
   PROPOSITO: imprime la lista
   PRECOND: ninguna, es una operacion total
*/
{
  if (isNil(xs))
   { cout << ("[]"); }
  else
   {
     Handle h = IniciarRecorrido(xs);
     cout << ("[ ");
     printElemType(elementoActual(xs,h));
     PasarAlSiguiente(xs,h);
     while (not finRecorrido(xs,h))
     {
       cout << (", ");
       printElemType(elementoActual(xs,h));
       PasarAlSiguiente(xs,h);
     }
     cout << (" ]");
     FinalizarRecorrido(xs,h);
   }
}

