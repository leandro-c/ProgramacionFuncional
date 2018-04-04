#include "Personas.h"

Persona crearPersona(string nombre, int edad){
    Persona p = new PersonasSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;
};
string getNombre(Persona p){
    return p->nombre;
};
void crecer(Persona& p){
    p->edad++;
};
