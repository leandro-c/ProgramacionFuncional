#include "auto.h"
Auto crearAutoVacio(Persona p1, Persona p2){
    Auto a = new AutoSt;
    a->piloto = p1;
    a->copiloto = p2;
    return a;
}

void subirPiloto(Auto& a, Persona p){
    a->piloto = p;
}
void bajarPiloto(Auto& a){
    a->piloto =  NULL;
}
void subirCopiloto(Auto& a , Persona p){
    a->copiloto = p;
}

void bajarCopiloto(Auto& a){
    a->copiloto = NULL;
}
void chocaElAutoYExplotaTodo(Auto& a){
    //delete a;
    //a = NULL; esto borra solo el auto
    muere(a->piloto);
    muere(a->copiloto);
    delete a
    a = NULL;

}
