#include <iostream>

using namespace std;
//in arbol se recorre con recursion

struct TNode{
    int elem;
    TNode* left;
    TNode* rigth;
};
typedef TNode* Tree;


//la interfaz es funcional
Tree emptyT();
bool isEmptyT(Tree t);
Tree leaf(int x);
Tree branch(int x,Tree t1,Tree t2);
int root(Tree t);
Tree left(Tree t);
Tree right(Tree t);
void destroyT(Tree& t);//borra todo el arbol
