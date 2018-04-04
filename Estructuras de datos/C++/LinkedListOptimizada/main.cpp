#include <iostream>
//#include "Usuario.h"
#include "listIterator.h"
using namespace std;

int main()
{
    //List l = cons(2,cons(4,cons(8,cons(10,nil()))));
    List xs = nil();
    mkCons(1,xs);
    mkCons(2,xs);
    mkCons(4,xs);
    mkSnoc(xs,8);
    imprimir(xs);
    //cout <<   << endl;
    //cout << todosIgual(8,l) << endl;

    //cout << "Hello world!" << endl;
    return 0;
}
