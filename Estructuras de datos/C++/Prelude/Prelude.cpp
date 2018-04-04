#include "Prelude.h"

using namespace std;

void printBool(bool b)
{ if (b) cout << "True" << endl; else cout << ("False") << endl; }

void printRespuesta(bool b)
{ if (b) cout << ("Si") << endl; else cout << ("No") << endl; }

bool esPar(int x) { return (x % 2 == 0); }
//int max(int x, int y) { return ((x>y)?x:y); }

