#include "Usuario.h"
#include <iostream>

using namespace std;

int longitud(List l){
    int len = 0;
    while(not isNil(l)){
            len++;
    }
    return len;
}

int sumatoria(List l){
    int sumatx = 0;
    while(not isNil(l)){
        sumatx += head(l);
        l = tail(l);
    }
    return sumatx;
}

bool todosIgual(int n, List l){
    bool todosIguales;
    while(not isNil(l)){
        int e = head(l);
        if(n == e){
            todosIguales = true;
        }
    }
}

bool algunoIgual(int n, List l){
     bool estasIgual;
    while(not isNil(l)){
        int e = head(l);
        if(n == e){
            estasIgual = true;
        }
    l = tail(l);
}
    return estasIgual;
}
