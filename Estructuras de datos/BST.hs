module BST(BSTTree,insertBST,perteneceBST,splitMinBST,splitMaxBST,elMaximoMenorA,elMinimoMayorA)where

data Tree a = EmptyT | NodeT a (Tree a)(Tree a)

import Tree

--Dado un BST inserta un elemento en el  arbol.
--Recursion por el arbol 
insertBST :: Eq a => a -> Tree a -> Tree a
--insertBST n EmptyT = ---
--insertBST  n (NodeT x t1 t2) = ----insertBST-----insertBST 
----miyagi
insertBST n EmptyT = (NodeT n EmptyT EmptyT)
insertBST  n (NodeT x t1 t2) = if(x > n)
								then NodeT x t1(insertBST n t2)
									else NodeT x (insertBST n t1) t2
if (n == x)
	then Nodet x t1 t2
	else if(x > n)
								then NodeT x t1(insertBST n t2)
									else NodeT x (insertBST n t1) t2
--inv.Rep.: Los arbol BST estan balanceados
--los elementos de t1 son menores que x
-- los elem. de t2 son mayores que x 
-- t1 y t2 son BST


--Dado un BST dice si el elemento pertenece o no al  ́arbol.
perteneceBST :: Ord a => a -> Tree a -> Bool
--perteneceBST es recursion por el arbol 
--perteneceBST n EmptyT = ---
--perteneceBST n (NodeT x t1 t2) = ---perteneceBST---pertencerBST---	
---miyagi 
perteneceBST n EmptyT = False
perteneceBST n (NodeT x _ _) = n == x
perteneceBST n (NodeT x t1 t2) = if(x < n)
									then perteneceBST n t1
									else x == e ||perteneceBST n t2



--Dado un BST devuelve un par con el mınimo elemento y el  arbol sin el mismo.
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--recursion por el arbol 
--splitMinBST EmptyT ---
--splitMinBST (NodeT x t1 t2) ---splitMinBST n t1 --- splitMinBST n t2 ---
--Miyagi
--precondicion el arblo no debe ser vacio
splitMinBST t = ((buscarElMinimo t),(sacarElMinimo t))  

buscarElMinimo::Tree a -> a
--precondicion no puede ser arbol vacio busacarElMinimo EmptyT 
buscarElMinimo (NodeT x EmptyT _) = x
buscarElMinimo (NodeT x t1 _) = busacarElMinimo t1


sacarElMinimo::Tree a -> -Tree a
sacarElMinimo (NodeT x EmptyT t2 ) = t2
sacarElMinimo (NodeT x t1 t2) = NodeT x (sacarElMinimo t1) t2
								


--Dado un BST devuelve un par con el m ́aximo elemento y el  ́arbol sin el mismo.
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--recursion por el arbol 
--splitMaxBST EmptyT ---
splitMaxBST (NodeT x t1 t2) = --- splitMaxBST t1 --- splitMaxBST t2 ---
--
splitMaxBST t = ((buscarElMaximo t),(sacarElMaximo t))

buscarElMaximo::Tree a -> a 
buscarElMaximo (NodeT x _ EmptyT)  = x
buscarElMaximo (NodeT _ _ t2) = busacarElMaximo t2

sacarElMaximo::Tree a -> -Tree a
--sacarElMaximo EmptyT = EmptyT
--sacarElMaximo (NodeT x EmptyT EmptyT) = EmptyT
sacarElMaximo (NodeT _ t1 EmptyT) = t1
sacarElMaximo (NodeT x t1 t2) = NodeT x t1 (sacarElMaximo t2)

--Dado un BST y un elemento, devuelve el maximo elemento que sea menor al elemento dado.
--recursion por el arbol, el arbol es BST , hay un elemento menor
elMaximoMenorA :: Ord a => a -> Tree a -> a
elMaximoMenorA a EmptyT = error 'Booom'
elMaximoMenorA a (NodeT x t1 t2) = if(a==x)
									then x
									else if(a < x)
										then elMaximoMenorA a t1
										else if(hayElementoMenorA a t2)
											then  elMaximoMenorA a t2
												else x

hayElementoMenorA::a->Tree a-> Bool
--recursion en el arbol prencon.: el arbol es BST
hayElementoMenorA n EmptyT = False
hayElementoMenorA n (NodeT x t1 t2) = (n >= x || hayElementoMenorA n t1)


------------------------------------------------------------------------------------------
elMaximoMenorA :: Ord a => a -> Tree a -> a
elMaximoMenorA e t = let m = manMenorMaybe e t
						in if(isNothing m)
							then error 'no hay menores a' 
								else fromJust m
maxMenorMaybe::Ord a => a -> Tree a -> Maybe 
maxMenorMaybe e ET = Nothing
maxMenorMaybe e (NT x t1 t2) = if(x < e)
								then maxDe x (maxMenorMaybe e t2)
								else maxMenorMaybe e t1
maxDe::a -> Maybe a -> Maybe a
maxDe x Nothing = x
maxDe x Just y = Just y																
------------------------------------------------------------------------------------------
elMaximoMenorA
--Dado un BST y un elemento, devuelve el m ́ınimo elemento que sea mayor al elemento dado.
elMinimoMayorA :: Ord a => a -> Tree a -> [a]






--AHORA CON MAYBE

--Dado un BST y un elemento, devuelve el maximo elemento que sea menor al elemento dado.
--recursion por el arbol, el arbol es BST , hay un elemento menor
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA a EmptyT = Nothing
elMaximoMenorA a (NodeT x t1 t2) = if(a==x)
									then Just x
									else if(a < x)
										then elMaximoMenorA a t1
										else if(hayElementoMenorA a t2)
											then  elMaximoMenorA a t2
												else Just x

hayElementoMenorA::a->Tree a-> Bool
--recursion en el arbol prencon.: el arbol es BST
hayElementoMenorA n EmptyT = False
hayElementoMenorA n (NodeT x t1 t2) = (n >= x || hayElementoMenorA n t1)