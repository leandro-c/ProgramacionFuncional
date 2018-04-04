#include "DestructiveLinkedLists.h"

struct lNode {
   ELEM_TYPE value;
   lNode* next;
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


void mkCons(ELEM_TYPE x, List& xs)
/*
   PROPOSITO: agrega el elemento x adelante de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: modifica xs
*/
{
  // Se pide memoria
  lNode* newNode = new lNode;
  // Se asignan los elementos
  newNode->value = x;
  newNode->next  = xs; // List es (struct lNode*)
  // Se retorna la lista recien construida
  xs = newNode;
}

void mkSnoc(List& xs, ELEM_TYPE x)
/*
   PROPOSITO: agrega el elemento x atras de la lista xs
   PRECOND: ninguna, es una operacion total
   OBSERVACIONES: modifica xs
*/
{
  List current;

  // Se pide memoria
  lNode* newNode = new lNode;
  // Se asignan los elementos
  newNode->value = x;
  newNode->next  = Nil();


  // Se calcula el ultimo elemento
  if (isNil(xs))
     xs = newNode;
  else
   {
     current = xs;
     while (not (current->next == NULL))
       current = current->next;

     current->next = newNode;
     // No hace falta asignar xs
     //  porque ya tenia el valor deseado
   }
}

bool isNil(List xs)
/*
   PROPOSITO:Tperacion total
*/
{ return (xs == NULL); }

ELEM_TYPE head(List xs)
/*
   PROPOSITO: devuelve el primer elemento de la lista
   PRECOND: xs no es vacia
*/
{ return (xs->value); }

void tkTail(List& xs)
/*
   PROPOSITO: devuelve la lista sin el primer elemento
   PRECOND: xs no es vacia
   OBSERVACIONES: modifica xs. Libera memoria
*/
{
    lNode* temp = xs;
    xs = xs->next;
    delete(temp);
}

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
     List ys = xs;
     cout << ("[ ");
     printElemType(ys->value);
     ys = ys->next;
     while (not isNil(ys))
     {
       cout << (", ");
       printElemType(ys->value);
       ys = ys->next;
     }
     cout << (" ]");
   }
}

List copiar(List xs)
/*
    PROPOSITO: devolver una copia de xs (que no comparta memoria)
    PRECOND: ninguna, es una operacion total
*/
{
   ListInt current = xs;
   ListInt newList = Nil();
   while (not isNil(current))
   {
      mkSnoc(newList, head(current));
      current = current->next;
   }
   return newList;
}
