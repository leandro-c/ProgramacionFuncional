#include "list2.h"

List nil(){
    return NULL;
}

bool isNil(List l){

   return l == NULL;

}


List cons(ELEM_TYPE x, List l){

        List  n = new lNode;
        n->elem = x;
        n->next = l;
        return n;
}

List tail(List l){
     return l->next;
}

ELEM_TYPE head(List l){
    return l->elem;
}

void imprimir(List l){

}

