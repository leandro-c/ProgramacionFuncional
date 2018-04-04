module ListaTree(Lista, 
	emptyL, 
	isEmptyL,
	headL,
	tailL,
	consL,
	appendL,
	lengthL) where

import Tree

data Lista a = MkLT (Tree a)

-- Inv. Rep.:
------ La rama izquierda de todos los nodos
------ es vacía

------ Siendo MkLT t una lista
------ Si t = NodeT x t1 t2
------ entonces t1 es EmptyT

emptyL :: Lista a
emptyL = MkLT EmptyT

isEmptyL :: Lista a -> Bool
isEmptyL (MkLT t) = isEmptyT t

headL :: Lista a -> a
headL (MkLT t) = raizRep t

consL :: a -> Lista a -> Lista a
consL x (MkLT t) = MkLT (agregarRep x t)

tailL :: Lista a -> Lista a
tailL (MkLT t) = MkLT (tailRep t)

appendL :: Lista a -> Lista a -> Lista a
appendL (MkLT t1) (MkLT t2) = 
	MkLT (appendRep t1 t2)

lengthL :: Lista a -> Int
lengthL (MkLT t) = sizeT t

raizRep (NodeT x _ _) = x

agregarRep x t = NodeT x EmptyT t

tailRep (NodeT _ _ t2) = t2

appendRep EmptyT t2          = t2
appendRep (NodeT x _ t1) t2  = 
	agregarRep x (appendRep t1 t2)