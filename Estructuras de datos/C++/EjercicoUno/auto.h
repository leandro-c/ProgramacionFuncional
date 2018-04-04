#include <iostream>
#include "Personas.h"
using namespace std;
struct AutoSt{
    Persona piloto;
    Persona copiloto;
};


typedef AutoSt* Auto;


Auto crearAutoVacio();
void subirPiloto(Auto& a, Persona p);
void bajarPiloto(Auto& a);
void subirCopiloto(Auto& a , Persona p);
void bajarCopiloto(Auto& a);
void chocaElAutoYExplotaTodo(Auto& a);
