import Prelude

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data NonEmptyList a = Unit' a | NECons a (NonEmptyList a)
data AppendList a = Nil | Unit a | Append (AppendList a) (AppendList a)
data T a = A a | B (T a) | C (T a) (T a)
data LTree a = L [a] | P a (LTree a) (LTree a)

data MTree a = H (Maybe a) | Y a (MTree a) (MTree a)
data GenTree a = GNode a [GenTree a]


-- EJERCICIO 1.1-------------------

----------------------------------------------------
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map f xs)
----------------------------------------------------
mapT:: (a -> b)-> Tree a -> Tree b
mapT f EmptyT  = EmptyT
mapT f (NodeT x t1 t2)  = NodeT (f x) (mapT f t1) (mapT f t2)
-----------------------------------------------------
mapNon :: (a -> b) -> NonEmptyList a -> NonEmptyList b
mapNon f (Unit' a) = Unit' (f a)
mapNon f (NECons a nel) = NECons (f a) (mapNon f nel)
------------------------------------------------------
mapAppL :: (a -> b) -> AppendList a -> AppendList b
mapAppL f Nil = Nil
mapAppL f (Unit a) = Unit (f a)
mapAppL f (Append apL1 apL2) = Append (mapAppL f apL1) (mapAppL f apL2)
-------------------------------------------------------------------------
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just(f a)
------------------------------------------------------------------------
mapX :: (a -> b) -> T a -> T b
mapX f (A a) = A (f a)
mapX f (B ta) = B (mapX f ta)
mapX f (C ta tb)= C (mapX f ta) (mapX f tb)
-----------------------------------------------------------------------
mapLtree :: (a -> b) -> LTree a -> LTree b
mapLtree f (L xs) = L (map' f xs)
mapLtree f (P a ma mb) = P (f a) (mapLtree f ma) (mapLtree f mb)
--------------------------------------------------------------------

mapEither :: (a -> b) -> Either c a -> Either c b
mapEither f (Left x) = Left x
mapEither f (Right y) = Right (f y)

--------------------------------------------------------------------

mapMTree :: (a -> b) -> MTree a -> MTree b
mapMTree f (H a) = H (mapMaybe f a)
mapMTree f (Y a t1 t2) = Y (f a) (mapMTree f t1) (mapMTree f t2)

-----------------------------------------------------------------

mapGenTree :: (a -> b) -> GenTree a -> GenTree b
mapGenTree f (GNode a xs) = GNode (f a) (map' (mapGenTree f) xs)

{--
-- EJERCICIO 1.2
---------------------------------------------------------------------------
--------------------Ejercicio 1.2.1----------------------------------------
---------------------------------------------------------------------------
mapX id = id

Por principio de extensionalidad

mapX id x = id x
---------------------------------------------------------------------------------------
Caso x = A a

mapX id (A a) = id (A a)
A (id a) = id (A a)  	por def de mapX_1
A a = A a 				por def de id en ambos terminos
---------------------------------------------------------------------------------
Caso x = B (T a)

HI: mapX id t1 = id t1
TI: mapX id (B t1) = id B (t1)

///////Primer termino
mapX id (B t1)
=			por def de mapX_2
B (mapX id t1)
= 			por hipotesis de induccion
B (id t1)
=			por def de id.
B t1
//////////Segundo Termino
Id (B t1)
=		Por def de id
B T1
-----------------------------------------------------------------------------
Caso x = C (T a) (T a)

HI1: mapX id t1 = id t1
HI2: mapX id t2 = id t2
TI: mapX id (C t1 t2) = id (C t1 t2)

//////////////Primer Termino
mapX id (C t1 t2)
= 				por def de map_3
C (mapX f t1) (mapX f t2)
=				por hipotesis de induccion
C (id t1) (id t2)
=				por def de id
C t1 t2
////////////Segundo Termino
id (C t1 t2)
=				por def de id
C t1 t2
---------------------------------------------------------------------------
--------------------Ejercicio 1.2.2----------------------------------------
---------------------------------------------------------------------------
mapX f . mapX g = mapX (f.g)

Por principio de extensionalidad

(mapX f . mapX g) x = mapX (f.g) x
sii				(.)
mapX f(mapX g x) = mapX (f.g) x
----------------------------------------------------------------------------
----------------------------------------------------------------------------
Caso x = A a

Primer termino
mapX f(mapX g (A a))
=			por def de mapX en el primer termino
mapX f (A (g a))
=		por def de mapX en el primer termino
A (f (g a))

Segundo Termino

mapX (f.g) (A a)
=
A ((f.g) a)
=			por def de (.)
A (f g a)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Caso x = B (T a)

HI: mapX f(mapX g t1) = mapX (f.g) t1
TI: mapX f(mapX g (B t1)) = mapX (f.g) (B(t1))

Primer Termino

mapX f(mapX g (B t1))
=			por def de mapX_2
mapX f (B (mapX g t1))
=			por def d mapX_2
B ((mapX f (mapX g t1))
=			por hipotesis de induccion
B (mapX (f.g) t1)

Segundo Termino

mapX (f.g) (B(t1))
=			por def de map
B (mapX (f.g) t1
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
Caso x = C (T a) (T a)

mapX f(mapX g x) = mapX (f.g) x

HI1: mapX f(mapX g t1) = mapX (f.g) t1
HI2: mapX f(mapX g t2) = mapX (f.g) t2
TI: mapX f(mapX g (C t1 t2)) = mapX (f.g) (C t1 t2)

Primer Termino

mapX f(mapX g (C t1 t2))
=			por def de mapX_3
mapX f (C (mapX g t1) (mapX g tb))
=			por def de mapX_3
C (mapX f (mapX g t1)) (mapX f (mapX g t2))
=			por hipotesis de induccion
C (mapX (f.g) t1) (mapX (f.g) t2)

Segundo Termino
mapX (f.g) (C t1 t2)
=		por def de mapX_3
C (mapX (f.g) t1) (mapX (f.g) t2)
-------------------------------------------------------------------------------
-------------------------------------------------------
--Entonces queda demostrado que en todos los casos los terminos son iguales
--}

-- EJERCICIO 1.3

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

findT :: (a -> Bool) -> Tree a -> Maybe a
findT p EmptyT = Nothing
findT p (NodeT x t1 t2) = if p x then Just x else  evalMaybe (findT p t1) (findT p t2)

evalMaybe :: Maybe a -> Maybe a -> Maybe a
evalMaybe Nothing Nothing = Nothing
evalMaybe Nothing (Just x) = Just x
evalMaybe (Just y) Nothing = Just y

findNEL :: (a -> Bool) -> NonEmptyList a -> Maybe a
findNEL p (Unit' a) = if p a then Just a else Nothing
findNEL p  (NECons a nel) = if p a then Just a else (findNEL p nel)

findAL :: (a -> Bool) -> AppendList a -> Maybe a
findAl p Nil = Nothing
findAL p (Unit a) = if p a then Just a else Nothing
findAL p (Append apl1 apl2) = evalMaybe (findAL p apl1) (findAL p apl2)

findM :: (a -> Bool) -> Maybe a -> Maybe a
findM p Nothing = Nothing
findM p (Just x) = if p x then Just x else Nothing

findX :: (a -> Bool) -> T a -> Maybe a
findX p (A a) = if p a then Just a else Nothing
findX p (B t1) = findX p t1
findX p (C t1 t2)=evalMaybe (findX p t1) (findX p t2)

findLT :: (a -> Bool) -> LTree a -> Maybe a
findLT p (L xs) = find p xs
findLT p (P x lt2 lt1) = if p x then Just x else  evalMaybe (findLT p lt1) (findLT p lt2)

findE :: (a -> Bool) -> Either b a -> Maybe a
findE p (Left y) = Nothing
findE p (Right x) = if p x then Just x else Nothing

findMT :: (a -> Bool) -> MTree a-> Maybe a
findMT p (H mb) = (findM p mb)
findMT p (Y a mt1 mt2) = if p a then Just a else evalMaybe (findMT p mt1) (findMT p mt2)

---------------------no estoy seguro de que este bien find de GenTree ------------------------------
findGT :: (a -> Bool) -> GenTree a -> Maybe a
findGT f (GNode a xs) = if f a then Just a else findGTList (findGT f) xs

findGTList :: (GenTree a -> Maybe a) -> [GenTree a] -> Maybe a
findGTList p [] = Nothing
findGTList p (x:xs) = if isNothing(p x) then findGTList p xs else p x

----------------------------------------------------------------------------------------------

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any p xs

anyT :: (a -> Bool) -> Tree a-> Bool
anyT f EmptyT = False
anyT f (NodeT x t1 t2) = f x || anyT f t1 || anyT f t2

anyNEL :: (a -> Bool) -> NonEmptyList a -> Bool
anyNEL f (Unit' a) = f a
anyNEL f (NECons a nel) = f a || (anyNEL f nel)

anyAL :: (a -> Bool) -> AppendList a -> Bool
anyAL f Nil = False
anyAL f (Unit a) = f a
anyAL f (Append apl1 apl2) = (anyAL f apl1) || (anyAL f apl2)

anyM :: (a -> Bool) -> Maybe a -> Bool
anyM f Nothing = False
anyM f (Just a) = f a

anyX :: (a -> Bool) -> T a -> Bool
anyX f (A a) = f a
anyX f (B t) = (anyX f t)
anyX f (C t1 t2) = (anyX f t1) || (anyX f t2)

anyLT :: (a -> Bool) -> LTree a -> Bool
anyLT f (L xs) = any f xs
anyLT f (P a lt2 lt1) = (f a) || (anyLT f lt1) || (anyLT f lt2)

anyE :: (a -> Bool) -> Either b a -> Bool
anyE f (Left x) = False
anyE f (Right y) = f y

anyMT :: (a -> Bool) -> MTree a -> Bool
anyMT f (H mb) = anyM f mb
anyMT f  (Y a mt1 mt2) = f a || (anyMT f mt1) || (anyMT f mt2)

anyGT :: (a -> Bool) -> GenTree a -> Bool
anyGT f (GNode a xs) = f a || (anyGTList (anyGT f) xs)

anyGTList :: (GenTree a -> Bool) -> [GenTree a] -> Bool
anyGTList f [] = False
anyGTList f (x:xs) = if f x then f x else (anyGTList f xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = p x && all p xs

allT :: (a -> Bool) -> Tree a-> Bool
allT f EmptyT = True
allT f (NodeT x t1 t2) = (f x) && (allT f t1) && (allT f t2)

allNEL :: (a -> Bool) -> NonEmptyList a -> Bool
allNEL f (Unit' a) = f a
allNEL f (NECons a nel) = f a && (allNEL f nel)

allAL :: (a -> Bool) -> AppendList a -> Bool
allAL f Nil = False
allAL f (Unit a) = f a
allAL f (Append apl1 apl2) = (allAL f apl1) && (allAL f apl2)

allM :: (a -> Bool) -> Maybe a -> Bool
allM f Nothing = False
allM f (Just a) = f a

allX :: (a -> Bool) -> T a -> Bool
allX f (A a) = f a
allX f (B t) = (allX f t)
allX f (C t1 t2) = (allX f t1) && (allX f t2)

allLT :: (a -> Bool) -> LTree a -> Bool
allLT f (L xs) = all f xs
allLT f (P a lt2 lt1) = (f a) && (allLT f lt1) && (allLT f lt2)

allE :: (a -> Bool) -> Either b a -> Bool
allE f (Left x) = False
allE f (Right y) = f y

allMT :: (a -> Bool) -> MTree a -> Bool
allMT f (H mb) = anyM f mb
allMT f  (Y a mt1 mt2) = f a && (allMT f mt1) && (allMT f mt2)

allGT :: (a -> Bool) -> GenTree a -> Bool
allGT f (GNode a xs) = f a && (allGTList (allGT f) xs)

allGTList :: (GenTree a -> Bool) -> [GenTree a] -> Bool
allGTList f [] = False
allGTList f (x:xs) = if f x then f x else (allGTList f xs)


partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([],[])
partition' f (x:xs) = let (t1,t2) = partition' f xs in if f x then ((x:t1),t2) else (t1,(x:t2))

partitionT :: (a -> Bool) -> Tree a-> ([a], [a])
partitionT p EmptyT = ([],[])
partitionT p (NodeT x t1 t2) = let (l1,l2) = concatParList (partitionT p t1) (partitionT p t2) in if p x then ((x:l1),l2) else (l1,(x:l2))

concatParList :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
concatParList (a,b) (c,d) = (a ++ b , c ++ d)

partitionNEL :: (a -> Bool) -> NonEmptyList a -> ([a], [a])
partitionNEL f (Unit' x) = if f x then ([x],[]) else ([],[x])
partitionNEL f (NECons x nel) = let (l1,l2) = (partitionNEL f nel) in if f x then ((x:l1),l2) else (l1,(x:l2))

partitionAL :: (a -> Bool) -> AppendList a -> ([a], [a])
partitionAL f Nil = ([],[])
partitionAL f (Unit x) = if f x then ([x],[]) else ([],[x])
partitionAL f (Append apl1 apl2) = concatParList (partitionAL f apl1) (partitionAL f apl2)

partitionM :: (a -> Bool) -> Maybe a -> ([a], [a])
partitionM f Nothing = ([],[])
partitionM f (Just x) = if f x then ([x],[]) else ([],[x])

partitionX :: (a -> Bool) -> T a -> ([a], [a])
partitionX f (A x) = if f x then ([x],[]) else ([],[x])
partitionX f (B t1) = partitionX f t1
partitionX f (C t1 t2) = concatParList (partitionX f t1) (partitionX f t2)

partitionLT :: (a -> Bool) -> LTree a -> ([a], [a])
partitionLT f (L xs) = partition' f xs
partitionLT f (P x lt1 lt2) = let (l1,l2) = concatParList (partitionLT f lt1) (partitionLT f lt2) in if f x then ((x:l1),l2) else (l1,(x:l2))

partitionE :: (a -> Bool) -> Either b a -> ([a], [a])
partitionE f (Left x) = ([],	[])
partitionR f (Right y) = if f y then ([y],[]) else ([],[y])

partitionMT :: (a -> Bool) -> MTree a -> ([a], [a])
partitionMT f (H mb) = partitionM f mb
partitionMT f (Y x mt1 mt2) = let (l1,l2) = concatParList (partitionMT f mt1) (partitionMT f mt2) in if f x then ((x:l1),l2) else (l1,(x:l2))


partitionGT :: (a -> Bool) -> GenTree a -> ([a], [a])
partitionGT f (GNode a xs) = let (l1,l2) = partitionGTList (partitionGT f) xs in if f a then ((a:l1),l2)  else (l1,(a:l2))

partitionGTList :: (GenTree a -> ([a],[a])) -> [GenTree a] -> ([a], [a])
partitionGTList f [] = ([],[])
partitionGTList f (x:xs) =  concatParList (f x) (partitionGTList f xs)



isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f []  = error "la lista esta vacia"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)


head' :: [a] -> a
head' = foldr (\x r -> x) (error "la lista ...")

tail' :: [a] -> [a]
tail' = recr (\x xs r -> xs) (error " ... ")

null' :: [a] -> Bool
null' = foldr (\x r -> False) True

sum' :: Num a => [a] -> a
sum' = foldr (+)0

product' :: Num a => [a] -> a
product' = foldr (*) 1

length' :: [a] -> Int
length' = foldr (\x r -> 1 + r) 0

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr(\x r -> x==y || r) False

notElem' :: Eq a => a -> [a] -> Bool
notElem' y = foldr(\x r -> x/=y && r) True

and' :: [Bool] -> Bool
and' = foldr (\ x r -> x && r) True

or' :: [Bool] -> Bool
or' = foldr (\ x r -> x || r) False

last' :: [a] -> a
last' = foldr1 (\x r -> r)

init' :: [a] -> [a]
init' = recr g (error "Lista Vacia ")
			where g x xs r = if (null' xs) then [] else x : r

reverse' :: [a] -> [a]
reverse' = foldr (\ x r -> r ++ [x]) []

subset' :: Eq a => [a] -> [a] -> Bool
subset' ys = recr g True
			where g x xs r = if (null' xs) then True else elem x ys && r

(+++) :: [a] -> [a] -> [a]
(+++) ys = foldr (\x r -> x: (ys ++ r) ) []

concat' :: [[a]] -> [a]
concat' = foldr (\x r -> x ++ r) []

(!!!) :: [a] -> Int -> a
(!!!) xs m = foldr g (error " lista vacia") xs m
			where g x r n = if n==0 then x else r (n-1)

take' :: Int -> [a] -> [a]
take' m xs = foldr g (\_ -> []) xs m
				where g x r n = if n == 0 then [] else x : r (n-1)

drop' :: Int -> [a] -> [a]
drop' m xs = foldr g (\_ -> []) xs m
   where g x r n = if n == 0 then [] else r (n-1)


zip' :: [a] -> [b] -> [(a,b)]
zip' xs (y:ys) = foldr (\x r -> (x,y) : r) [] xs -- PREGUNTAR


--splitAt :: Int -> [a] -> ([a],[a])
--splitAt m xs = foldr g (\_ -> ([],[])) m
	--	where g x r n = if n == 0 then ([], r) else let (ys, zs) = r (n-1) in (x:ys, zs)

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
minimum' :: Ord a => [a] -> a
minimum' = recr g (error "...")
   where g x xs r = if null xs then x else min x r
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' k = foldr g Nothing
				where g x r = if k == (fst x) then Just (snd x) else r
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr (\x r -> (((fst x): fst r),((snd x): snd r))) ([],[])
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
tails' :: [a] -> [[a]]
tails' = recr g []
			where g x xs r = (x:xs):r ++ []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
--replicate' :: Int -> a -> [a]
--replicate' m a = foldr g (\_ -> [])  m
				--where g x r n = if n == 0 then x else a: r (n-1) \\preguntar. es opcional y se hace con unfoldr
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
--repeat' :: a -> [a]
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
cycle' :: [a] -> [a]
cycle' = foldr (\_ r -> r ++ r) []   -- preguntar
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
group' :: Eq a => [a] -> [[a]]
group' = recr g []
			where g x xs r = if null xs then [[x]] else agrupo x r

agrupo :: Eq a => a -> [[a]] -> [[a]]
agrupo v (y:ys) = if v == (head y) then (v:y): ys else [v] : (y : ys)
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
intersperse' :: a -> [a] -> [a]
intersperse' a = recr g []
					where g x xs r = if null xs then [x] else x:a:r
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
delete' :: Eq a => a -> [a] -> [a]
delete' y = foldr (\x r -> if x == y then r else x:r) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
insert' :: Ord a => a -> [a] -> [a]
insert' y = recr g [y]
				where g x xs r = if x > y then y:x:xs else x:r
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
nub' :: (Eq a) => [a] -> [a]
nub' = foldr (\x r -> if elem' x r then r else x:r) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
elemIndex' :: Eq a => a -> [a] -> Maybe Int
elemIndex' a = foldr (\x r -> if x == a then Just 0 else Just (sumar1Just r)) Nothing

sumar1Just :: Maybe Int -> Int
sumar1Just (Just n) = 1 + n
sumar1Just Nothing = 0
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' a xs = agregarSiEncuentra a (index' xs)

agregarSiEncuentra :: Eq a => a ->[(Int,a)] -> [Int]
agregarSiEncuentra a = foldr (\x r -> let ys = r in if a==(snd x) then (fst x):ys else ys)[]
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
mapFoldr' :: (a -> b) -> [a] -> [b]
mapFoldr' f = foldr (\x r -> f x : r) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr decidirSiAgregar []
   where decidirSiAgregar x r =
           if f x then x : r else r
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr f = foldr (\x r -> f x || r) False
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr f 	= foldr (\x r -> f x && r) True
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
findFoldr :: (a -> Bool) -> [a] -> Maybe a
findFoldr f = foldr (\x r -> if f x then Just x else r) Nothing
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
partitionFoldr :: (a -> Bool) -> [a] -> ([a],[a])
partitionFoldr f = foldr (\x r -> let (ys,zs) = r in if f x then ((x:ys),zs) else (ys,(x:zs))) ([],[])
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
nubByFoldr :: (a -> a -> Bool) -> [a] -> [a]
nubByFoldr f = foldr (\x r -> let zs = r in if(any (f x) zs) then zs else (x:zs)) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
deleteByFoldr :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteByFoldr f a = foldr (\x r -> if f x a then r else x:r)[]
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
groupByFoldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldr f = recr g []
					where g x xs r = if null xs then [[x]] else let (zs:zss) = r in if (f x (head zs)) then ((x:zs):zss) else [x] : (zs:zss)
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
concatMapFoldr :: (a -> [b]) -> [a] -> [b]
concatMapFoldr f = foldr (\x r -> f x ++ r) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
sort' :: Ord a => [a] -> [a]
sort' = foldr (\x r -> insert' x r) []

-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
--until' :: (a -> Bool) -> (a -> a) -> a -> a unfold
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x r -> if f x then x : r else []) []
----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f = foldr (\x r -> if f x then r else x:r) []
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' f = recr g ([],[])
			where g x xs r = if f x then (x:(fst r),snd r) else ([],x:xs)
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
break' :: (a -> Bool) -> [a] -> ([a],[a])
break' f = recr g ([],[])
			where g x xs r = if f x then ([],x:xs) else (x:(fst r),snd r)
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
--zipApply' :: [(a -> b)] -> [a] -> [b]
--zipApply' (y:ys) = foldr g (\_ _ -> [] ) ys
				--where g x r zs = x y : r zs


-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
index' :: [a] -> [(Int,a)]
index' = foldr (\x r -> (0,x) : sumar1aLaLista r) []

sumar1aLaLista :: [(Int,a)] -> [(Int,a)]
sumar1aLaLista = foldr (\x r -> (sumarAlPar x) : r) []

sumarAlPar :: (Int,a) -> (Int,a)
sumarAlPar (x,y) = (1+x,y)
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f = foldr (esUnIndice f) Nothing

esUnIndice :: (a -> Bool) -> a -> Maybe Int -> Maybe Int
esUnIndice f x r = if f x then Just 0 else Just (sumar1Just r)

-----------------------------------------------------------------------------------------------------------------
{--

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f []  = error "la lista esta vacia"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

--}
{--
repeat :: a -> [a] unfold
cycle :: [a] -> [a]
nats :: [Int] unfold
inits :: [a] -> [[a]]


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf :: Eq a => [a] -> [a] -> Bool

until :: (a -> Bool) -> (a -> a) -> a -> a //unfold

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipApply :: [(a -> b)] -> [a] -> [b]


iterate :: (a -> a) -> a -> [a] // unfold



\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\DEMOSTRACIONES CON FOLDR\\\\\\\\\\\\\\\\\\\\\\\\\\'

-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
////// EJERCICIO 1. concat = foldr (++) []//////////

concat = foldr (++) []

Por principio de extensionalidad

concat x = foldr (++) [] x


Caso base x = []

concat [] = foldr (++) [] []

///Primer Termino
concat []
=		por def de concat
[]

///Segundo Termino
foldr (++) [] []
=
[]

Caso inductivo x = (h:hs)

HI: concat xs = foldr (++) [] xs
TI : concat (x:xs) = foldr (++) [] (x:xs)

///Primer Termino
concat (x:xs)
=		x def de concat
(++) x (concat xs)


///Segundo Termino
foldr (++) [] (x:xs)
=		x def de foldr
(++) x (foldr (++) [] xs)
=		por hipotesis de induccion
(++) x (concat xs)

Entonces queda demostrado que para el caso base y el inductivo ambos terminos son iguales.
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

////// EJERCICIO 2. map f = foldr ((:) . f) [] //////////

Por principio de extensionalidad
¿map f x = foldr ((:) . f ) [] x?

Caso base x = []

map f [] = foldr ((:) . f ) [] []

///Primer Termino
map f []
=		por def de map_1
[]

///Segundo Termino
foldr ((:) . f ) [] []
=		por def de foldr
[]


Caso inductivo x = h:hs

HI: map f xs = foldr ((:) . f ) [] xs
TI : map f (x:xs) = foldr ((:) . f ) [] (x:xs)

///Primer Termino
map f xs
=		por def de map_2
f x : map f xs

///Segundo Termino
foldr ((:) . f ) [] (x:xs)
=			por def de foldr
((:) . f ) x (foldr ((:) . f ) [] xs)
= 		por def de (.)
((:) (f x) x (foldr ((:) . f ) [] xs)
=		por def de (:)
f x : (foldr ((:) . f ) [] xs)
=		por hipotesis de induccion
f x : map f xs

Entonces queda demostrado que para el caso base y el inductivo ambos terminos son iguales.
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

////// EJERCICIO 3. foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs //////////

Caso base x = []

foldr f z ([] ++ ys) = foldr f (foldr f z ys) []

///Primer Termino
foldr f z ([] ++ ys)
=		por def (++)
foldr f z (ys)

///Segundo Termino
foldr f (foldr f z ys) []
=		por def de foldr_1
(foldr f z ys)


Caso inductivo x = h:hs

HI: foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs
TI: foldr f z ((x:xs) ++ ys)  = foldr f (foldr f z ys) (x:xs)

///Primer Termino
foldr f z ((x:xs) ++ ys)
=		por def de ++
foldr f z (x:(xs ++ y))
= 	por def de foldr
f x (foldr f z (xs ++ y))

///Segundo Termino
foldr f (foldr f z ys) (x:xs)
=		por def del primer foldr_2
f x (foldr f (foldr f z ys) xs)
=		por hipotesis de induccion
f x foldr f z (xs ++ ys)

Entonces queda demostrado que para el caso base y el inductivo ambos terminos son iguales.
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------

////// EJERCICIO 4- foldr f z . foldr (:) [] = foldr f z //////////

Por principio de extensionalidad

¿foldr f z . foldr (:) [] x = foldr f z x?
sii 		(.)
¿foldr f z (foldr (:) [] x) = foldr f z x ?

Caso Base x = []

foldr f z (foldr (:) [] []) = foldr f z []

///Primer Termino
foldr f z (foldr (:) [] [])
=		por def del segundo foldr_1
foldr f z []
=
[]

///Segundo Termino
foldr f z []
=		por def de foldr
[]


Caso inductivo x = h:hs

HI: foldr f z (foldr (:) [] xs) = foldr f z xs
TI: foldr f z (foldr (:) [] (x:xs)) = foldr f z (x:xs)

///Primer Termino
foldr f z (foldr (:) [] (x:xs))
=			por def de foldr
foldr f z ((:) x (foldr (:) [] xs))
=		acomodo terminos
foldr f z (x:(foldr (:) [] xs))
=
f x (foldr f z (foldr (:) [] xs))
=		por hipotesisde induccion
f x (foldr f z xs)


///Segundo Termino
foldr f z (x:xs)
=		por def de foldr
f x (foldr f z xs)

Entonces queda demostrado que para el caso base y el inductivo ambos terminos son iguales.
-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
--}
















