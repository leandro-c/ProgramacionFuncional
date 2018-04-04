#include "Prelude.h"

#define STACK_ELEM_TYPE char
#define printStackElemType(x) { printf("'%c'", x); }

/* Declaraciones de tipos -- NO USAR! Deben abstraerse! */
struct sNode {
   STACK_ELEM_TYPE value;
   struct sNode*   rest;
};
typedef struct sNode* Stack;

/****************************/
/* Operaciones sobre stacks */
/****************************/
/* OBSERVACION: 
     no se hacen suposiciones sobre el gasto o 
     el nivel de reuso de memoria. Debe observarse
     la implementacion.
*/

// La pila vacia
Stack emptyStack();

// Agregar un elemento a la pila
Stack push(STACK_ELEM_TYPE x, Stack p); 

// Ver si es la pila vacia
Bool isEmptyStack(Stack p);

// Primer elemento y resto de una pila no vacia
STACK_ELEM_TYPE top(Stack p);      // Parcial en la pila vacia
Stack pop(Stack p);          // Parcial en la pila vacia

// Para imprimir pilas de manera piola
void printStack(Stack p);

