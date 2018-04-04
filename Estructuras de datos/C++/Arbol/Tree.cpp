#include <Tree.h>

Tree emptyT(){
    return NULL;
}
bool isEmptyT(Tree t){
    return t == NULL;
}
Tree leaf(int x){
    TNode* a = new Tree;
    a->elem = x;
    a->left = NULL;
    a->rigth = NULL;
    return a;
}
Tree branch(int x,Tree t1,Tree t2){
    TNode* a = new Tree;
    a->elem = x;
    a->rigth = t1;
    a->left = t2;
    return a;
}
int root(Tree t){
    return t->elem;
}
Tree left(Tree t){
    return t->left;
}
Tree right(Tree t){
    return t->rigth;
}
void destroyT(Tree& t){
    delete t;
    t = NULL;
}
