#include <iostream>
typedef int ELEM_TYPE;

using namespace std;

struct lNode{
    ELEM_TYPE elem;
    lNode* next;
};
typedef lNode* Iterator;
struct lHeader{
    lNode* first;
    lNode* last;
    int len;
};
typedef lHeader* List;
//Crea una lista vacia
List nil();
//Indica si la lista es vacia
bool isNil(List xs);

//Agrega un elemento al final de la lista
void mkCons(ELEM_TYPE x,List& xs);
//Agrega un elemento al fin al de la lista
void mkSnoc(List& xs,ELEM_TYPE x);

//Funciones para recorrer la lista con iteradores
Iterator getIterator(List xs);//retorna un iterador que apunta al primero.
bool atEnd(Iterator it);//retorna el elemento en la posicion.
ELEM_TYPE getElem(Iterator it);//retorna el elemento en la posicion.
void setElem(Iterator& it,ELEM_TYPE x);//setea el elemento en la posicion
void moveToNext(Iterator& it);// desplaza el iterador al proximo
void destroyIt(Iterator& it);//destruye al iterador
void imprimir(List xs);
//Duplica la lista en otra memoria
List copiar(List xs); //realiza una copia de todos los nodos de la lista
