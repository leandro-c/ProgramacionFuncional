#include <stdio.h>
#include <stdlib.h>

#include "TestingLists.h"

/* MAIN */
int main()
{
  List xs = Nil();
  mkCons(13, xs);
  mkCons( 2, xs);
  mkCons( 2, xs);
  mkCons(42, xs);
  mkCons( 1, xs);
  testingLists(xs);
}

