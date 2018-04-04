
/* aca van los headers */
#include <stdio.h>
#include <string.h>

#include "Prelude.h"
#include "TestingQueues.h"

/* TESTING */
void testingQueues()
{
  Queue p = emptyQueue();
  printf("\n");
  printf("p=emptyQueue     = "); printQueue(p); printf("\n");
  encolar('2',p);
  printf("p=encolar('2',p) = "); printQueue(p); printf("\n");
  encolar('3',p);
  printf("p=encolar('3',p) = "); printQueue(p); printf("\n");
  printf("elQueSigue(p)    = "); printQueueElemType(elQueSigue(p)); printf("\n");
  descolar(p);
  printf("p=descolar(p)    = "); printQueue(p); printf("\n");
}
