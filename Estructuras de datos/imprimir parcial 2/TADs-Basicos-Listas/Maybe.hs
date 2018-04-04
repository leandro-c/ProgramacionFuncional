eje::[a]
eje [a,b,c,d,f,h,x,y,z]

--Dada una lista quita su  ́ultimo elemento.
init2 :: [a] -> [a]
init2 [] = Nothing
init2 [x] = Just x
init2 xs = reverse(head(reverse xs))

--Dada una lista, devuelve su  ́ultimo elemento.
last :: [a] -> a
last [] = Nothing 
last xs = Just(head(reverse(xs)))


--Dado un elemento y una lista devuelve la posici ́on de la lista en la que se encuentra
--dicho elemento.
--indiceDe :: Eq a => a -> [a] -> Int


--Dada una lista de pares (clave, valor) y una clave devuelve el valor asociado a la clave.
--valorParaClave :: Eq k => [(k,v)] -> k -> v


--Dada una lista de elementos devuelve el m ́aximo.
--maximum :: Ord a => [a] -> a


--Dado un  ́arbol devuelve su elemento m ́ınimo.
--minT :: Ord a => Tree a -> a
--mintT::Tree a ->Maybe a
--mintT Empty = Nothing
--mintT (NodeT x t1 t2) = minMaybe (Just x) (minMaybe (mintT t1)(mintT t2))

--minMaybe:: Ord a => Maybe a -> Maybe a -> Maybe a
--minMaybe Nothing y = y
--minMaybe x Nothing = x
--minMaybe (Just x) (Just x) = Just(min x y)
