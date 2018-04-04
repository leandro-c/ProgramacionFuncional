#include "..\Prelude\Prelude.h"

#define ELEM_TYPE int
#define printElemType(x) { printf("%d", x); }

/* Declaraciones de tipos -- NO USAR! Deben abstraerse! */
struct lNode {
   ELEM_TYPE     value;
   lNode* next;
};

struct lHeader {
    lNode* firstElem;
    lNode* lastElem;
    lNode* currentElem;
    int currentId;
    int nextId;
    /* INVARIANTE DE REPRESENTACION:
        * firstElem == NULL sii lastElem == NULL
        * currentId == 0 si no hay recorrido en curso
        * nextId nunca es 0
    */
};

typedef lHeader* List;
    /* INVARIANTE DE REPRESENTACION:
        * la lista es siempre != NULL
    */
typedef int Handle;

/****************************/
/* Operaciones sobre listas */
/****************************/
/* OBSERVACIONES:
   * no se hacen suposiciones sobre el gasto o
     el nivel de reuso de memoria. Debe observarse
     la implementacion.
   * la operacion de Snoc se implementa funcional
*/

// La lista vacia
List Nil();
bool isNil(List xs);

// Agregar un elemento adelante o atras y separarlo
void mkCons(ELEM_TYPE x, List& xsref);
void mkSnoc(List& xsref, ELEM_TYPE x);
ELEM_TYPE splitHead(List& xsref);      // Parcial en la lista vacia

// Funciones para recorrer la lista
Handle IniciarRecorrido(List& xsref);
bool finRecorrido(List xs, Handle h);
ELEM_TYPE elementoActual(List xs, Handle h);
void PasarAlSiguiente(List& xsref, Handle h);
void FinalizarRecorrido(List& xsref, Handle h);

// Duplica la lista en otra memoria
List copiar(List xs);

// Operaciones de modificacion de listas
void mkInsert(int x, List& xsref);
void mkAppend(List& xs, List ys);
void mkDump(List& xs, List& ys);

// Para imprimir listas de manera piola
void printList(List xs);

