#include "..\Prelude\Prelude.h"
#include "AppendableListsRecorribles.h"

//************************
// Estructurales
// Iterativas
int ilength(List xs);               // Longitud
List ireverse(List xs);             // Lista en orden inverso
bool found(List xs, int& xref);     // Busca x y lo modifica segun lo que encuentra

// Recursivas
int length(List xs);
List reverse(List xs);

//***************************
// Para listas de enteros
typedef List ListInt;
ListInt isuccl(ListInt xs);         // Lista con uno mas en cada elemento
List succl(List xs);

/* TESTING */
void testingLists(ListInt xs);
