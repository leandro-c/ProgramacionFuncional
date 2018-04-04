typedef int ELEM_TYPE
#include <iostream>

using namespace std;
Struct LStack{
    ELEM_TYPE elem;
    LStack* next;
}
typedef LStack*Stack

Stack emptyS();

bool isEmptyS(Stack s);

void push(int x, Stack& s);

int top(Stack s);

void pop(Stack& s);

int size(Stack s);

void destroyS(Stack& s);
