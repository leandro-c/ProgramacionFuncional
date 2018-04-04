
list nill(){
    return NULL;
}

bool isNil(list l){

   return l == NULL;

}


list cons(ELEM_TYPE x, list l){

        Lnode  n = new Lnode;
        n->elem = x;
        n->next = l;
        return n;
}

list tail(list l){
     return l->next;
}

ELEM_TYPE head(list l){
    return l->elem;
}

void imprimir(list l){

    for_each
}
