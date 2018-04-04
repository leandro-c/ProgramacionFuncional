#include <iostream>
using namespace std;

struct PersonasSt{
    string nombre;
    int edad;
};
typedef PersonasSt* Persona;

Persona crearPersona(string nombre, int edad);

void crecer(Persona& p);
