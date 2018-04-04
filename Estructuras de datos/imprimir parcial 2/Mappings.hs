module Mappings(Map, 
	emptyM, assocM, lookupM, removeM, domM) where

import Set

data Tree a = ET | NT a (Tree a)(Tree a)

data Map k v = M (Tree(k,v))
--Inv.Rep.: La lista no tien claves repetidas-
---tengo que tener las claves ordenadas.
----el Arbol es BST

emptyM::Map k v
emptyM = M ET

assocM::Map k v -> k -> v ->Map k v
assocM (m t) k v = M(agregarBST t k v)


agregarBST::Tree(k,v)->k->v->Tree(k,v)
--Precondicion: el arbol es BST
agregarBST ET = NT(ki,vi) ET ET
agregarBST (NT(kr,vr) t1 t2) ki vi = if ( ki == kr)
										then NT(kr,vi)t1 t2
										else if(ki < kr)
											then NT (kr,vr)(agregarBST t1 kv vr) t2
											else NT(kr,vr)t1 (agregarBST t2 kv vr)

lookupM::Map k v -> k -> Maybe v
lookupM (M t) k = M(buscarBST k)											
--
buscarBST ::Tree(k,v) -> k ->Maybe v
--recursion por arbol
buscarBST ET  k = Nothing
buscarBST (NT(kr,vr)t1 t2) kb = if(kb == kr)
	then Just vs
	else if(kb < kr)
		then buscarBST t1 kb
			else buscarBST t2 kb

domM::Map k v -> Set k
domM (M t)	= claves t

claves::Tree(k,v) -> Set k
claves ET = emptyS
claves (Nt(kr,vr)t1 t2) = unionS(unionS(claves t1)(claves t2))(singleton kr)

deleteM:: Map k v -> k -> v -> Map k v 
deleteM (M t) = k v = M(borrarBST t k v)

borrarBST::Tree(k,v)->k->Tree(k,v)
borrarBST (NT(kr,vr)t1 t2)kb = if(kb == kr)
	then rearmarBST t1 t2
	else if(kb >kr)
		then NT(kr,vr)(borrarBST t1 kb)t2
		else NT(kr,vr)t1(borrarBST t2 kb)
