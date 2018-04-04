#include <stdio.h>
#include <stdlib.h>

#include "TestingLists.h"

/* MAIN */
int main()
{
  ListInt xs = Cons(1, Cons(42, Cons(2,
                 Cons(2, Cons(13, Nil())))));
  //testingLists(xs);
  //cout << "xs = ";
  //printList(xs);
  //cout << endl;
  //cout << "sum(xs) = " << isum(xs);

  cout << ("Verificando problemas de single threadedness...\n");

  xs = Cons(1, Cons(42, Cons(2,
         Cons(2, Cons(13, Nil())))));
  ListInt ys = Snoc(xs, 17);



  cout << ("xs = Cons(1, Cons(42, Cons(2, Cons(2, Cons(13, Nil()))))) = "); printList(xs); cout << ("\n");
  cout << ("ys = Snoc(xs, 17) = ");
           printList(ys); cout << endl;
  cout << ("xs = ");
           printList(xs); cout << endl;

  return(0);
}

