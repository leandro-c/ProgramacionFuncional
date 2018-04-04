#include <iostream>

struct SetNode{
    int x;
    SetNode* next;
};

typedef SetNode*Set;

Set emptyS();
Set singleton(int x);
bool belongs(int x, Set s);
Set addS(int x, Set s1);
Set removeS(int x, Set s1);
Set unionS(Set s1,Set s2);
void destroyS(Set& s);
