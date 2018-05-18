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



