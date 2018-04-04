module BinaryHeap(Heaps,emptyH,isEmptyH,insertH,findMin,deleteMin,splitMin)where



--inv. de Representacion.: 
---1: x es menor que los elementos de t1 y t2 
----2: t1 y t2 son heaps
-----3: el arbol es completo
------4:el arbol se completa de izq a derecha
data Heap a = H(Tree a)[Dir]
data Tree a = ET | NT (Tree a)a(Tree a)
data Dir = Izq | Der

import Tree

emptyH :: Heap a --0(n)
emptyH = H ET []


isEmptyH :: Heap a -> Bool
isEmptyH (H t _) = isEmptyT t

insertH :: Ord a => a -> Heap a -> Heap a
insertH n (H ET []) = 
insertH n (H t dirs ) = H(insertarEnTree n t (reverse dirs))(nextPos occ)

--prop: dado un elem un t y una lista de dir me retorna un elem con un minimo en la raiz.
insertarEn::Ord a => a -> Tree a -> [Dir] -> Tree a
insertarEn e emptyT [] = NT e ET ET  
insertarEn e (NT x t1 t2)(Izq :dirs) = flotarIzq e (insertarEn e t1 dirs)	t2
insertarEn e (NT x t1 t2)(Der :dirs) = flotarDer e t1 (insertarEn e t2 dirs)


flotarIzq::Ord a => a -> Tree a -> Tree a -> Tree a
flotarIzq x t1@(NT x' t1' t2') t2 = if (x'<x)
									then NT x' (NT x t1' t2')t2
										else NT x t1 t2
nextPos::[Dir]->[Dir]
nextPos [] = [Izq]
nextPos (Izq:ds) = Der : ds
nextPos	(Der:ds) = Izq : (nextPos ds)




findMin :: Ord a => Heap a -> a -- Parcial en emptyH

deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
deleteMin ( H t ds ) = let pos =  prevPos ds
						in
							H (deleteMinEnTree t (reverse pos)) pos

deleteMinEnTree::Ord a => a -> Tree a -> [Dir] -> Tee a
deleteMinEnTree t  ds = let (last tree) = splitLast t ds
							in if(isEmpty tree)
								then emptyT
									else decantar last (tIzq tree)(tDer tree)							
decantar									


splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH