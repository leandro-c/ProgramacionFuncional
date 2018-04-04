#include "Set.h"
Set emptyS(){
    //Set s = new Set
    return NULL;
}
Set singleton(int y){
    //SetNode* s = new SetNode;
    Set s = new SetNode;
    s->x = y;
    s->next = emptyS();
    return s;
}
bool belongs(int y, Set s1){
    while(s1 != NULL){
        if(s1->x == y){
            return true;
        }
    }
    return false;
}
Set addS(int y, Set s1){
    Set s = new SetNode;
    s->x = y;
    s->next = emptyS();

    if(not belongs( y s)){
        s1->next = s;
    }
}
Set removeS(int y, Set s1){
    while(s1 != NULL){
        if(s1->x == y){
            delete s1;
            s1 = NULL;
        }
    }
}
Set unionS(Set s1,Set s2){

}
void destroyS(Set& s){
    while(not s == NULL){
        Set tem =  s ;
        s = s->next;
        delete tem;
    }

}
