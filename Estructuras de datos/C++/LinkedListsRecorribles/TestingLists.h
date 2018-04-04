#include "..\Prelude\Prelude.h"
#include "LinkedListsRecorribles.h"

//************************
// Estructurales
// Iterativas
int ilength(List xs);               // Longitud
List ireverse(List xs);             // Lista en orden inverso
List iappend(List xs, List ys);     // Append de dos listas
// Recursivas
int length(List xs);
List reverse(List xs);
List append(List xs, List ys);

//***************************
// Para listas de enteros
typedef List ListInt;
ListInt isuccl(ListInt xs);         // Lista con uno mas en cada elemento
List succl(List xs);

/* TESTING */
void testingLists(ListInt xs);
