using namespace std;
#include "..\Prelude\Prelude.h"
#define ELEM_TYPE int

/* Declaraciones de tipos -- NO USAR! Deben abstraerse! */
struct lNode;
typedef lNode* List;

/****************************/
/* Operaciones sobre listas */
/****************************/
/* OBSERVACIONES:
   * no se hacen suposiciones sobre el gasto o
     el nivel de reuso de memoria. Debe observarse
     la implementacion.
   * la operacion de Snoc es destructiva!!!!
     Si su argumento no es singleThreaded, da problemas
*/

// La lista vacia
List Nil();

// Agregar un elemento adelante o atras
List Cons(ELEM_TYPE x, List xs);
List Snoc(List xs, ELEM_TYPE x);

// Ver si es la lista vacia
bool isNil(List xs);

// Primer elemento y resto de una lista no vacia
ELEM_TYPE head(List xs);      // Parcial en la lista vacia
List tail(List xs);           // Parcial en la lista vacia

// Para imprimir listas de manera piola
void printList(List xs);

