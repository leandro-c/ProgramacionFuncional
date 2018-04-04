int sumT(Tree t){
    if(isEmptyT(t)){
        return 0;
    }else{
        return root(t) +sumT(left(t))
                        +sumT(right(t));
    }
}

List levelN(int n,Tree t){
    if(n == 0){
        List xs = create();
        add(root(t),xs);
        return xs;
    }else{
        return append(levelN(n-1),left(t),levelN(n-1,right(t)));
    }
}
List levelN(int n,Tree t){
    List xs = create();
    levelNAux(n,t,xs);
    return xs;
}

void levelNAux(int n, Tree t,List& xs){

}
