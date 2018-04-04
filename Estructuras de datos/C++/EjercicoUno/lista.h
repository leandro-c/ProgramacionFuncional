//data List a = [] | a :[a]

struct NodoL{
    int elem;
    NodoL*next;
}
typedef NodoL*List;

//interfaz funcional

List nil();
List cons(int x, List xs);
List tail(List xs);
int head(List xs);

bool isNil(List xs);
