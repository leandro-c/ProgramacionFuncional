typedef int ELEM_TYPE

Struct lNode{
    ELEM_TYPE elem;
    Lnode* next;
}
typedef Lnode* list

list nill();

bool isNil(list l);

list cons(ELEM_TYPE x, list l);

list tail(list l);

elem_type head(list l);

void imprimir(list l);
