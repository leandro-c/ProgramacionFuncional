#include "Prelude.h"

/* Declaraciones necesarias para generalizar pares */
#define ELEM_TYPE_X int
#define printElemX(x) { printf("%d", x); }

#define ELEM_TYPE_Y int
#define printElemY(y) { printf("%d", y); }

/* Declaraciones de tipos -- NO USAR! Deben abstraerse! */
struct PairNode {
  ELEM_TYPE_X x;
  ELEM_TYPE_Y y;
};
typedef struct PairNode* Pair;

/***************************/
/* Operaciones sobre pares */
/***************************/
// Construccion de un par
Pair MkPair(ELEM_TYPE_X a, ELEM_TYPE_Y b);

// Acceso a los elementos de un par
ELEM_TYPE_X fst(Pair p);
ELEM_TYPE_Y snd(Pair p);

// Mostrando pares por pantalla de manera bonita
void printPair(Pair p);

