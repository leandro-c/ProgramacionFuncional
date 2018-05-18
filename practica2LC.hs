--a) 
apply' :: (a -> b) -> a -> b
apply' f x = f x
--En Haskell se llama ($)
--b) 
twice :: (a -> a) -> a -> a
twice   f x     =   f (f x)
--c) 
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x 
---d) 
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g a  = f(g a)
--e) 
curry :: ((a,b) -> c) -> a -> b -> c
curry f a b = f(a,b)
--f ) 
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b
--g) 
map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map f xs
--h) 
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (a:as) = if f a then a:filter f as else filter f as
--i) 
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (a:as) = f a || any f as

esSiete:: Int->Bool
esSiete x = x == 7

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = False
all' f (a:as) = f a && all' f as
--j) 
maibe :: b -> (a -> b) -> Maybe a -> b
maibe b f Nothing = b
maibe  b f (Just a) = f a
----k) chequear
--either' :: (a -> c) -> (b -> c) -> Either a b -> c
--either' f g (Right a )    =   f  a 
--either' f g (Left b )     =   g  b
----l) 
find' :: (a -> Bool) -> [a] -> Maybe a
find' f []   =   Nothing
find' f (x:xs)   =   if (f x) then (Just x ) else find' f xs
----m) 
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f []  =   ([],[])
partition f (x:xs)  =   if (f x) then (x:fst(partition f xs),snd(partition f xs)) else (fst(partition f xs ),x:snd(partition f xs))
----n) 
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy f []  =   []
nubBy f  (x:xs) = if (elem_by f x xs) then  x:(nubBy f xs) else nubBy f xs

nub:: (Eq a) => [a] -> [a]
nub []  = []
nub (x:xs) = if elem x xs then nub xs else x: nub xs  
  
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []         =  False
elem_by eq y (x:xs)     =  x `eq` y || elem_by eq y xs 
----ñ) 
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (x:xs) = if f a x then xs else x : deleteBy' f a xs
--deleteBy' (<=) 4 [1..10]

----o) 
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f [] = []
groupBy f [x] = [[x]]
groupBy f (x:xs) =  let (a:as) = groupBy f xs in if  (f x  (head a))  then (x:a):as else [x]:a:as
--groupBy (\x y -> (x*y `mod` 3) == 0) [1,2,3,4,5,6,7,8,9]

----p) 
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f [] = [] 
concatMap' f (x:xs) = f x ++ concatMap f xs 

----q) chequear
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' f g a = if (f a) then a else until' f g (g a) 

----r ) 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if (f x) then x:takeWhile' f xs else takeWhile' f xs 

----s) 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if (f x) then dropWhile' f xs else xs
--dropWhile (< 9) [1,2,3]
--dropWhile' (< 3) [1,2,3,4,5,1,2,3]
----t) 
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) = if (f x) then (x:fst(span' f xs),snd(span' f xs)) else ([],(x:xs))

--span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
--span (< 9) [1,2,3] == ([1,2,3],[])
--span (< 0) [1,2,3] == ([],[1,2,3])

break' :: (a -> Bool) -> [a] -> ([a],[a])
break' f [] = ([],[])
break' f (x:xs) = let (zs,ws) = break' f xs in if not (f x) then (x:zs,ws) else ([],(x:xs))

-- break' (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4]) 
-- break' (< 9) [1,2,3] == ([],[1,2,3]) 
-- break' (> 9) [1,2,3] == ([1,2,3],[])
----u) 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _     = []
zipWith' f _ []     = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- zipWith' (+) [1,2,3] [3,2,1]    

--v) 
zipApply :: [(a -> b)] -> [a] -> [b]
zipApply [] _ = []  
zipApply (f:fs) (x:xs) = (f x) : zipApply fs xs
---- zipApply [(> 3)] [1,2,3,4,5,6,7,8,9]
--w) 
index' :: [a] -> [(Int,a)]
index' [] = []
index' (x:xs) = (0,x): sumToList (index' xs)

sumToList :: [(Int,a)] -> [(Int,a)]
sumToList [] = []
sumToList (x:xs) = sumarPair x : sumToList xs

sumarPair :: (Int,a) -> (Int,a)
sumarPair (x,y) = (1+x,y)

--x ) 

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f x = x
applyN n f x = f (applyN (n-1) f x)

--y)
--iterate f x == [f x, f (f x), f (f (f x)), ...] 

iteratee :: (a -> a) -> a -> [a]
iteratee f x = (iteratee f x) 

--
--z ) 
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f [] = Nothing
findIndex f (x:xs) = if (f x) then Just 0 else Just (sumJust(findIndex f xs) )

sumJust :: Maybe Int -> Int
sumJust (Just n) = 1 + n
sumJust Nothing = 0

--------------------------
--2.1 AAAAALTO ORDEN AMEO
--------------------------

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

elem':: Eq a => a -> [a] -> Bool  
elem' = (.) not . (.) isNothing . find' . (==) 

notElem :: Eq a => a -> [a] -> Bool
notElem  = (.) not . (.) isNothing . find' . (/=)

and' :: [Bool] -> Bool  
and' = all' id 

or' :: [Bool] -> Bool
or' = any' id

subset :: Eq a => [a] -> [a] -> Bool
subset = flip (all . flip elem)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f =  (.) f (applyN (n-1) f)

(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) [] y = y
(+++) (x:xs) ys = x:(+++)xs ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x (+++) concat' xs

zip' :: [a] -> [b] -> [(a,b)] 
zip' = zipWith' (,) 
--unzip
replicate':: Int -> a -> [a]
replicate' n = take n . repeat
--inits 
--isSuffixOf
elem':: Eq a => a -> [a] -> Bool
elem' x  =  any' (x==)

elemIndex' :: Eq a => a -> [a] -> Maybe Int
elemIndex' a = findIndex' (==a)

group:: Eq a => [a] -> [[a]]
group = groupBy (==)

delete :: (a -> a -> Bool) -> a -> [a] -> [a]
delete = deleteBy' (==)

nub:: [a]->[a]
nub = nubBy (==)

--zipApply = zipWith apply

--index = zipWith (\xy -> (x,y))[0 ..] xs

--findIndex::(a->Bool)->[a]->Maybe int
--findIndex p  = 
        --maybe Nothing (Just . snd) .
