#include <iostream>
typedef int ELEM_TYPE;


using namespace std;


struct lNode{
    ELEM_TYPE elem;
    lNode* next;
};
typedef lNode* List;

List nil();

bool isNil(List l);

List cons(ELEM_TYPE x, List l);

List tail(List l);

ELEM_TYPE head(List l);

void imprimir(List l);

