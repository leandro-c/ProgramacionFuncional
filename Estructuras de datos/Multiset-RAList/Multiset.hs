module Multiset(Multiset, emptyMS, occursMS, addMS, removeMS, unionMS, intersectMS) where

import Map
import Maybe

data Multiset a = MkMS (Map a Int)
-- Inv. Rep.:
-- Los valores del map son mayores a 0

-- Crea un multiset
emptyMS :: Multiset a -- O(1)
emptyMS = MkMS emptyM

-- Devuelve la cantidad de apariciones 
-- de un elemento
occursMS :: Ord a => a -> Multiset a -> Int -- O(n)
occursMS x (MkMS m) = lookupInt x m

-- Añade una aparición de un elemento
addMS :: Ord a => a -> Multiset a -> Multiset a -- O(n)
addMS x (MkMS m) = MkMS (addOccurs x m)

-- Decrementa la cantidad de apariciones de un elemento
removeMS :: Ord a => a -> Multiset a -> Multiset a --O(n)
removeMS x (MkMS m) = MkMS (removeOccurs x m)

--- TAREA

-- Une dos multiset (suma las ocurrencias de cada elemento 
-- entre ambos multiset)
unionMS :: Ord a => Multiset a -> Multiset a -> Multiset a
unionMS (MkMS m1) (MkMS m2) = if(isEmptyM m1)
				then (MkMS m2)
				else if(isEmptyM m2)
					then (MkMS m1)
					else MkMS(unirMaps m1 m2)
-- Se queda con la mínima ocurrencia de cada elemento entre dos multiset
-- (observación: si un elemento no ocurre en uno de los multiset
-- entonces no aparece en el resultado)
intersectMS :: Ord a => Multiset a -> Multiset a -> Multiset a
intersectMS (MkMS m1) (MkMS m2) = if(isEmptyM m1)
									then emptyMS
									else if(isEmptyM m2)
										then  emptyMS
										else MkMS(interseccionMaps m1 m2)

-------------------------------------------------------

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing  = 0
maybeToInt (Just n) = n

lookupInt :: Ord a => a -> Map a Int -> Int
lookupInt x m = maybeToInt (lookupM x m) 

addOccurs :: Ord a => a 
               -> Map a Int -> Map a Int 
addOccurs x m =
	assocM x (lookupInt x m + 1) m

removeOccurs :: Ord a => a -> Map a Int -> Map a Int
removeOccurs x m =
	unaOccurMenos x (lookupInt x m) m

unaOccurMenos :: Ord a => a -> Int -> Map a Int -> Map a Int
unaOccurMenos x 0    m = m
unaOccurMenos x 1    m = removeM x m
unaOccurMenos x n    m = assocM x (n-1) m

unirMap :: Ord a => Map a Int -> Map a Int -> Map a Int
unirMap m1 m2 = recorrerMap (setToList (domM m1)) m1 m2

recorrerMap :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
recorrerMap [] _ map2 = map2 
recorrerMap (x:xs) map1 map2 = if isNothing (lookupM x (recorrerMap xs map1 map2)) 
          						then assocM x (fromJust(lookupM x map1)) (recorrerMap xs map1 map2) 
          						else assocM x ((fromJust(lookupM x map1))+ (fromJust((lookupM x map2)))) (recorrerMap xs map1 map2)

interseccionMaps::Ord a => Map a Int -> Map a Int -> Map a Int
interseccionMaps m1 m2 = intersecarMap (setToList (domM m2)) (setToList (domM m2))  m2 
--intersec une dos listas y me da una con los mismo elementos.

intersecarMap::Ord a => [a]->[b]->Map a Int -> Map a Int-- -> Map a Int
intersecarMap [][]m1  = emptyM
intersecarMap (x:xs)(y:ys) m = if(x == y)
									then assocM x (fromJust(lookupM x map1))(intersecarMap xs ys m)         						
									else 