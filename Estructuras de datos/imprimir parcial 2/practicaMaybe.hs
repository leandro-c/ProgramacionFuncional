eje::[Int]
eje = [9,8,7,6,5,4]

data Map k v = MkM [(k, v)]
--Dada una lista quita su  ́ultimo elemento.
--
init2 :: [a] -> Maybe[a]
init2 [] = Nothing
init2 [x] = Just[]
init2 (x:xs) = consMaybe x (init2 xs) 

consMaybe ::a -> Maybe [a] -> Maybe [a]
consMaybe a Nothing = Just[a]
consMaybe a (Just xs) = Just(a:xs) 

--Dada una lista, devuelve su  ́ultimo elemento.
last2 :: [a] -> Maybe a
last2 [] = Nothing 
last2 [x] = Just x
--last xs = Just(head(reverse(xs)))
last2  (x:xs) = last2 xs

--Dado un elemento y una lista devuelve la posici ́on de la lista en la que se encuentra
--dicho elemento.
indiceDe :: Eq a => a -> [a] -> Maybe Int
indiceDe a [] =  Nothing
indiceDe a (x:xs) = if(a == x)
then Just 1
else sumUnoSiPuede (indiceDe a xs)

sumUnoSiPuede :: Maybe Int -> Maybe Int
sumUnoSiPuede Nothing = Nothing
sumUnoSiPuede (Just y) = Just 1 + y

--Dada una lista de pares (clave, valor) y una clave devuelve el valor asociado a la clave.
--valorParaClave :: Eq k => [(k,v)] -> k -> v


--Dada una lista de elementos devuelve el m ́aximo.
--maximum :: Ord a => [a] -> a
--maximum [] = Nothing
--maximum [x] = Just x
--maximum (x:xs) = if max x > head xs
--					then x
--					else maximum xs


--Dado un  ́arbol devuelve su elemento m ́ınimo.
--minT :: Ord a => Tree a -> Maybe a
--mintT::Tree a ->Maybe a
--mintT Empty = Nothing
--mintT (NodeT x t1 t2) = minMaybe (Just x) (minMaybe (mintT t1)(mintT t2))

--minMaybe:: Ord a => Maybe a -> Maybe a -> Maybe a
--minMaybe Nothing y =  y
--minMaybe x Nothing =  x
--minMaybe (Just x) (Just y) = Just(min x y)


--++Map
--module Map(Map,emptyM,assocM,lookUpM,deleteM,domM)where
--emptyM::Map k v
--assocM::Eq k =>Map k v -> k -> v ->Map k v
--lookUpM::Eq k => Map k v -> k -> Maybe v
--deleteM:: Eq k => Map k v ->k-> Map k v
--domM:: Map k v -> Set k

pedirTel::[String] -> Map String Int -> [Maybe Int]
pedirTel [] map = []
pedirTel (n:ns) map = (lookUpM map n) : (perdirTel ns map)


ocurrencias:: [Char] -> Map Char Int
ocurrencias [] = EmptyM
ocurrencias (c:cs) = let rec = ocurrecias cs
			in assocM  rec c (incrementarV c rec)

incrementarV ::Char -> Map Char Int -> Int 
incrementarV c map = let rec =lookUpM c map 
						in if isNothing rec
						then 1
						else 1 + (fromJust rec)
--if isNothing (lookUpM c map)then 1 else 1 + (fromJust(lookUpM c map))




indexar :: [a] -> Map Int a
indexar [] = emptyM
indexar (x:xs) =  assocM x  (indiceDe x xs(indexar xs))
