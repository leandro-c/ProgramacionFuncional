module Tree where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

isEmptyT :: Tree a -> Bool
isEmptyT EmptyT = True
isEmptyT _      = False

esHoja :: Tree a -> Bool
esHoja (NodeT _ EmptyT EmptyT) = True
esHoja _      = False

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) =
	1 + max (heightT t1) (heightT t2)

hojas :: Tree a -> [a]
hojas EmptyT = []
hojas t@(NodeT x t1 t2) =
	if esHoja t
        then x : hojas t1 ++ hojas t2
        else hojas t1 ++ hojas t2

-- Etc...
-- Todas funciones sobre Tree