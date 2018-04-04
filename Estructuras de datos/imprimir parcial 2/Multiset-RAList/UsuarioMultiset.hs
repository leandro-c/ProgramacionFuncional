import Multiset
import Queue

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Tarea:

-- O(n^2)
-- Cuenta la cantidad de ocurrencias de cada elemento
-- en la lista, utilizando un Multiset
ocurrencias :: Ord a => [a] -> Multiset a
ocurrencias xs = undefined

ocurrenciasT :: Ord a => Tree a -> Multiset a
ocurrenciasT t = undefined

ocurrenciasQ :: Ord a => Queue a -> Multiset a
ocurrenciasQ q = undefined
