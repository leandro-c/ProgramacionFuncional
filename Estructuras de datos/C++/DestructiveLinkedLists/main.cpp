#include <iostream>
#include "TestingLists.h"

using namespace std;

/* MAIN */
int main()
{
  List xs = Nil();
  mkCons(13, xs);
  mkCons(2, xs);
  mkCons(2, xs);
  mkCons(42, xs);
  mkCons(1, xs);
  //testingLists(xs);

  cout << "Verificando problemas de single threadedness...\n";

  xs = Nil();
  mkCons(13, xs);
  mkCons(2, xs);
  mkCons(2, xs);
  mkCons(42, xs);
  mkCons(1, xs);
  cout << "xs = "; printList(xs); cout << endl;
  mkSnoc(xs, 17);
  cout << "xs = "; printList(xs); cout << endl;

}

