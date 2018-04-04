#include <stdio.h>
#include "Pairs.h"

// Construccion de pares -- aloca memoria nueva
Pair MkPair(ELEM_TYPE_X a, ELEM_TYPE_Y b)
{
 Pair node = malloc(sizeof(struct PairNode));
 node->x = a;
 node->y = b;
 return node;
}

// Acceso a los elementos del par
ELEM_TYPE_X fst(Pair p)
{ return (p->x); }

ELEM_TYPE_Y snd(Pair p)
{ return (p->y); }

// Mostrando los pares de manera bonita
void printPair(Pair p)
  // Asume que las funciones printElemX 
  //  y printElemY fueron definidas en Pairs.h
{ 
  printf("(");
  printElemX(fst(p));
  printf(", ");
  printElemY(snd(p)); 
  printf(")");
}

