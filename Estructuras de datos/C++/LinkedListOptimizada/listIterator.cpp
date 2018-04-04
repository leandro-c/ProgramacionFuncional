#include "listIterator.h"
//Crea una lista vacia

/*
    inv.Rep.: el lHeader no tiene que ser vacio
*/
List nil(){
    lHeader* h = new lHeader;
    h->first = NULL;
    h->last = NULL;
    h->len = 0;
    return h;
}
//Indica si la lista es vacia
bool isNil(List xs){
   return xs->last == NULL;
}
//Agrega un elemento al principio de la lista
void mkCons(ELEM_TYPE x,List& xs){
        //creo que nuevo nodo
        lNode*  n = new lNode; //new reserva memoria en heap. siempre con * asi me dice en que lugar de memoria ir
        n->elem = x;
        n->next = xs->first;
        //modifico el header
        if(isNil(xs)){
            xs->last = n;
        }
        xs->len ++;
        xs->first = n;

}
//Agrega un elemento al fin al de la lista
void mkSnoc(List& xs,ELEM_TYPE x){
    lNode*  n = new lNode;
    n->elem = x;
    n->next = NULL;
    if(xs->last == NULL){
        xs->first = n;
    }else{
        xs->last->next = n;
    }
    xs->last = n;
    xs->len ++;
}
//Funciones para recorrer la lista con iteradores
Iterator getIterator(List xs){//retorna un iterador que apunta al primero.
    return xs->first;
}
bool atEnd(Iterator it)//retorna el elemento en la posicion.
{
    return it->next == NULL;
}
ELEM_TYPE getElem(Iterator it)//retorna el elemento en la posicion.
{
    return it->elem;
}
void setElem(Iterator& it,ELEM_TYPE x)//setea el elemento en la posicion
{
    it->elem = x;
}
void moveToNext(Iterator& it)// desplaza el iterador al proximo
{
    it = it->next;
}
void destroyIt(Iterator& it)//destruye al iterador
{
    it = NULL;
}
void imprimir(List xs){
    Iterator n = getIterator(xs);
    while(not atEnd(n)){
            cout << getElem(n) << endl;
            moveToNext(n);
    }
    cout << getElem(n) << endl;
    destroyIt(n);
}
//Duplica la lista en otra memoria
List copiar(List xs)//realiza una copia de todos los nodos de la lista
{
    lHeader* aux = new lHeader;
    while(isNil(xs)){
        mkCons(xs->first->elem,aux);
        moveToNext(xs->first);
    }
    return aux;
}
