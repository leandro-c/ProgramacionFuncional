--recr::b -> ( a -> b -> b ) -> [a] -> b
--recr z f [] = z
--recr z f ( x : xs' ) = f x xs' (recr z f xs')
----------------------------------------------------------------------------------------------------------------
foldr ::(a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [] = error "..."
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs) xs
--Users/leandrocasarin/Desktop/entrega5.hs

--maximum = recr g (error "...")
--where x xs r =
--    if null xs
--       then x
--       else max x r

----------------------------------------------------------------------------------------------------------------
head'::[a] -> a
head' xs = foldr (\x r -> x ) error"lista vacia.." xs
--head' = foldr (\x r -> x ) error"lista vacia.."

sum':: Num a => [a] -> a
sum' xs = foldr (\x r -> x + r) 0 xs
-- sum' xs = foldr (+) 0 xs

elem':: Eq a => a -> [a] -> Bool
elem' a xs = foldr (\x r -> a == x || r) False xs

notElem':: Eq a => a -> [a] -> Bool
notElem' a xs = foldr (\x r -> a /= x || r) True xs

and':: [Bool] -> Bool
and' xs = foldr (\x r -> x && r ) True xs

or' :: [Bool] -> Bool
or' xs = foldr (\x r -> x || r ) False xs

last':: [a] -> a
last' xs = (\x r -> r ) error "no podes pedir algo que no hay"  xs

-------------------
init' :: [a] -> [a]
init' xs = foldr (\x r -> x : r ) [] xs
-------------------
-------------------
subset' :: Eq a => [a] -> [a] -> Bool
subset' xs ys = foldr(\x r -> ) True xs
--subset' []  []= True
--subset' xs [] = False
--subset' [] ys = True
--subset' (x:xs) (y:ys) =  elem x (y:ys) && subset' xs ys
--------------------
(+++) :: [a] -> [a] -> [a]
(+++) ys = foldr(\x r -> x : ( ys ++ r )) []
--(+++) [] [] = []
--(+++) [] y = y
--(+++) (x:xs) ys = x:(+++)xs ys

concat' :: [[a]] -> [a]
concat' xxs = foldr ( \x r -> x ++ r ) [] xxs
--concat' [] = []
--concat' (x:xs) = x ++ concat' xs

(!!!) :: [a] -> Int -> a
(!!!) xs n = foldr g error"..." xs n
where g x r n =
  if x == 0
    then x
    else r (n-1)
--(!!!) [] n = error "estas pidiendo indice de una lista vacia"
--(!!!) (x:_) 0 = x
--(!!!) (xs) n = (!!!) xs (n-1)
take' :: Int -> [a] -> [a]
take' m xs = foldr g (\_ -> []) xs m
where g x r n =
  if n == 0
    then []
    else x : r (n-1)

--Donde, me olvidé de decir, (\_ -> []) se puede reemplazar por const []

--take' :: Int -> [a] -> [a]
--take' n [] = error "no podes tomar nada de una lista vacia"
--take' 1 (x:xs) = [x]
--take' n (x:xs) = if n <= 0 then [] else x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' m xs = foldr g (\_ -> []) xs m
where g x r n =
  if n == 0
    then r
    else r (n-1)

--drop' n [] = []
--drop' n (x:xs) =  if n <= 0
--                    then xs
--                     else  drop' (n-1) xs

zip'::[a] -> [b] -> [(a,b)]
zip' xs (y:ys) = foldr (\x r -> (x,y) : r ) [] xs
--zip' [] [] = []
--zip' xs [] = []
--zip' [] xs = []
--zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)
maximum' :: Ord a => [a] -> a
maximum' = recr g (error "...")
where g x xs r =
  if null xs
    then x
    else max x r


tail' :: [a] -> [a]
tail' = recr g []
where g x xs r =
(x:xs) : r ++ []
--tails' :: [a] -> [[a]]
--tails' [] = []
--tails' (x:xs) = ((x:xs):(tails' xs)) ++ []
null'' :: [a] -> Bool
null'' = foldr (\x r -> False) True

length' :: [a] -> Int
length' = foldr (\x r -> 1 + r ) 0


snoc :: [a] -> a -> [a]
snoc xs a = foldr(\x r -> r : x : a ) []

reverse' :: [a] -> [a]
reverse' xs = foldr(\x r -> r ++ [x] ) []

subset' :: Eq a => [a] -> [a] -> Bool
subset' ys = recr g True
where g x xs r =
  if (null xs)
    then True
      else elem' x ys && r

--no se como reemplazar con las funciones que hicmos antes...
--splitAt' :: Int -> [a] -> ([a], [a])
--splitAt' n xs = foldr(\x r -> )
--splitAt' :: Int -> [a] -> ([a],[a])
--splitAt' n xs = ((take' n xs), (drop' n xs))

unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr(\x r -> ((fst x): fst r),((snd x): snd r))) []

tailss :: [a] -> [[a]]
tailss = recr g []
where g x xs r = (x:xs):r ++ []

nub' :: (Eq a) => [a] -> [a]
nub' = foldr (\x r -> if elem' x r then r else x:r)

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex a = foldr(\x r -> if x==a then Just 0 else Just(justSumar r)) Nothing

justSumar :: Maybe Int -> Int
justSumar (Just n) = 1 + n
justSumar Nothing = 0

group' :: Eq a => [a] -> [[a]]
group' = recr g []
where g x xs r =
  if null' xs
    then [[x]]
      else agrupar x r

agrupar::Eq a => a -> [[a]] -> [[a]]
agrupar y (x:xs) =
if y ==(head x)then (y:x):xs else [y]:(x:xs)
delete' :: Eq a => a -> [a] -> [a]
delete' z = foldr(\x r -> if z==x then r else x:r) []

mapp :: (a -> b) -> [a] -> [b]
mapp f = foldr(\x r -> fx:r)[]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr(\x r -> if f x then x:r else r ) []

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr(\x r -> f x || r) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr(\x r -> f x && r) True

find' :: (a -> Bool) -> [a] -> Maybe a
find' f = foldr(\x r -> if f x then Just x else r)Nothing

--countBy' :: (a -> Bool) -> [a] -> Int
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f = foldr(\x r -> let (ys,xs) = r in if f x then ((x:ys),xs) else (ys,(x:xs))) ([],[])

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr(\x r -> f x ++r)[]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x r -> if f x then x : r else []) []

--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
{--}
--Este lo hizo fede asi que mucho vuelta no le doy
{-
maximum':: Ord a => [a] -> a
maximum' [] = error "no hay maximum en lista vacia"
maximum' [a] = a
maximum' (x:xs) = if x > (maximum' xs) then x else (maximum' xs)

minimum' :: Ord a => [a] -> a
minimum' [] = error "no hay minimun en lista vacia"
minimum' [a] = a
minimum' (x:xs) = if x < (minimum' xs) then x else (minimum' xs)

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' k [] = Nothing
lookup' k ((k2, v) : kvs) = if k == k2 then Just v  else lookup'  k kvs
--
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xs) = let (list1, list2) = (unzip' xs) in (x:list1, y:list2)
--


replicate' :: Int -> a -> [a]
replicate' n a = a : replicate (n-1) a

--tiene sentido esto?
repeat' :: a -> [a]
repeat' n = n:(repeat' n)

--lo mismo , tiene sentido?
cycle' :: [a] -> [a]
cycle' xs = xs ++ (cycle' xs)

nats :: [Int]
nats = [1..]
--Ejemplo: agrupar [1,1,2,2,1,3,3,3] == [[1,1],[2,2],[1],[3,3,3]]
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar [x] = [[x]]
agrupar (x:xs) =
    let (a:as) = agrupar xs
        in if x == head a  then (x:a):as else [x]:a:as
-}