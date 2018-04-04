#include "Prelude.h"

#define QUEUE_ELEM_TYPE char
#define printQueueElemType(x) { printf("'%c'", x); }

/* Declaraciones de tipos -- NO USAR! Deben abstraerse! */
struct qNode {
   QUEUE_ELEM_TYPE value;
   struct qNode*   back;
};
struct qTop {
   // INVARIANTE: first == NULL sii last == NULL
   struct qNode* first;
   struct qNode* last;
};
// INVARIANTE: una cola nunca es NULL
typedef struct qTop* Queue;

/****************************/
/* Operaciones sobre colas  */
/****************************/
/* OBSERVACION: 
     no se hacen suposiciones sobre el gasto o 
     el nivel de reuso de memoria. Debe observarse
     la implementacion.
*/

// La cola vacia
Queue emptyQueue();

// Agregar un elemento a la cola (por atras... :-).
void encolar(QUEUE_ELEM_TYPE x, Queue q); 

// Ver si es la cola vacia
Bool isEmptyQueue(Queue q);

// Primer elemento y resto de una cola no vacia
QUEUE_ELEM_TYPE elQueSigue(Queue q);      // Parcial en la cola vacia
void desencolar(Queue q);                 // Parcial en la cola vacia

// Para imprimir colas de manera piola
void printQueue(Queue q);

