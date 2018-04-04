#include "..\Prelude\Prelude.h"
#include "SimpleLinkedLists.h"

//************************
// Estructurales
//
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
// Iterativas
int icount(ListInt xs, int x);      // Cantidad de elementos iguales a x
int isum(ListInt xs);               // Suma de los elementos
int imaximum(ListInt xs);           // Maximo elemento
ListInt isuccl(ListInt xs);         // Lista con uno mas en cada elemento
ListInt ipares(ListInt xs);         // Solo los pares de la lista original

ListInt zipAdd(ListInt xs, ListInt ys); // Devuelve la lista de las sumas elemento a elemento
List inub(List xs);                     // Eliminacion de repetidos
List iposPares(List xs);                // Elementos en las posiciones pares
bool verifSumAnteriores(List xs);       // Verifica que todos los elementos sean suma de sus anteriores
bool verifOrdenada(List xs);            // Verifica que la lista este ordenada
bool expandeA(ListInt xs, ListInt ys);  // Verifica que xs expanda a ys

// Recursivas
int count(List xs, int searchFor);
int sum(List xs);
List succl(List xs);
List pares(List xs);

/* TESTING */
void testingLists(ListInt xs);
