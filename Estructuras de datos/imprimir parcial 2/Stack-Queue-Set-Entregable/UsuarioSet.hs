import Set

-- O(n^2)
-- porque listToSet es O(n^2)
-- aunque setToList es O(1)
sacarRepetidos :: Eq a => [a] -> [a] 
sacarRepetidos xs = setToList (listToSet xs)

-- O(n^2)
-- porque add es O(n) y recorro toda una lista
listToSet :: Eq a => [a] -> Set a
listToSet [] = emptySet
listToSet (x:xs) = add x (listToSet xs)