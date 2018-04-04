#include <stdio.h>
#include <math.h>

#include "TestingPairs.h"

/* TESTING */
void testingPairs(Pair p)
{
  printf("p           = "); printPair(p); printf("\n");
  printf("fst(p)      = %d\n", fst(p));
  printf("snd(p)      = %d\n", snd(p));
  printf("padd(p,p)   = "); printPair(padd(p,p)); printf("\n");
  printf("distance(p) = %f\n", distance(p));
}

Pair padd(Pair p1, Pair p2)
{ return (MkPair(fst(p1)+fst(p2), snd(p1)+snd(p2))); }

float distance(Pair p)
{ return (sqrt((fst(p)^2+snd(p)^2))); }

