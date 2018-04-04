#include <stdio.h>
#include <stdlib.h>

#include "TestingLists.h"

/* MAIN */
int main()
{
  ListInt xs = Cons(1, Cons(42, Cons(2, Cons(2, Cons(13, Nil())))));
  //testingLists(xs);

  printf("Verificando problemas de single threadedness...\n");

  xs = Cons(1, Cons(42, Cons(2, Cons(2, Cons(13, Nil())))));
  printf("xs = Cons(1, Cons(42, Cons(2, Cons(2, Cons(13, Nil()))))) = "); printList(xs); printf("\n");
  ListInt ys = Snoc(xs, 17);
  printf("ys = Snoc(xs, 17) = "); printList(ys); printf("\n");
  printf("xs = "); printList(xs); printf("\n");

}

