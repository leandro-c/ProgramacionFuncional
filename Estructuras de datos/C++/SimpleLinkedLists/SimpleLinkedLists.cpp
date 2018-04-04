#include "..\Prelude\Prelude.h"
#include "SimpleLinkedLists.h"

using namespace std;

struct lNode {
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
   OBSERVACIONES: MODIFICA xs como efecto lateral y
                  todas las sublistas compartidas!
*/
{
  List current;
  List retorno;

  // Se pide memoria
  lNode* newNode = new lNode;
  // Se asignan los elementos
  newNode->value = x;
  newNode->next  = NULL;


  // Se calcula el ultimo elemento
  if (isNil(xs))
     retorno = newNode;
  else
   {
     current = xs;
     while (not (current->next == NULL))
       current = current->next;

     current->next = newNode;
     retorno = xs;
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

void printList(List xs)
/*
   PROPOSITO: imprime la lista
   PRECOND: ninguna, es una operacion total
*/
{
  if (isNil(xs))
   { cout << "[]"; }
  else
   {
     List current = xs;
     cout << "[ ";
     cout << head(current);
     current = tail(current);
     while (not isNil(current))
     {
       cout << ", ";
       cout << head(current);
       current = tail(current);
     }
     cout << " ]";
   }
}

