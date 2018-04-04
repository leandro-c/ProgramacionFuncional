import RAList

-- TAREA

-- Pasa una lista a una RAList
listToRAList :: [a] -> RAList a -- O(n^2)
listToRAList xs = undefined

-- Busca el índice que posee un elemento en una RAList
-- Prec.: el elemento existe en la lista
indexOf :: a -> RAList a -> Int -- O(n^2)
indexOf x rl = undefined

-- Devuelve los elementos que están en los
-- índices tomados por parámetro
-- Prec.: los indices existen
elemsEn :: [Int] -> RAList a -> [a] -- O(n^2)
elemsEn xs rl = undefined