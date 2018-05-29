apply :: (a -> b) -> a -> b
apply f x = f x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

(...) :: (b -> c) -> (a -> b) -> a -> c
(...) f g x = f (g x)


curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)  

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = (f x) y 

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if (f x) then x:filter' f xs else  filter' f xs

any', all' :: (a -> Bool) -> [a] -> Bool

any' f [] = False
any' f (x:xs) = (f x) || (any' f xs)

all' f [] = False
all' f (x:xs) = (f x) && (any' f xs)

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' x f Nothing = x
maybe' x f (Just y) = (f y)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left y) = f y
either' f g (Right y) = g y

find' :: (a -> Bool) -> [a] -> Maybe a
find' f [] = Nothing
find' f (x:xs) = if f x then Just x else (find' f xs)

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f [] = ([],[])
partition f (x:xs) = let (t1,t2) = partition f xs in if f x then ((x:t1),t2) else (t1,(x:t2))

nubBy' :: (a -> a -> Bool) -> [a] -> [a]
nubBy' f [] = []
nubBy' f (x:xs) = let ys = (nubBy' f xs) in  if (any' (f x) ys) then ys else x:ys

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a] 
deleteBy' f x [] = []
deleteBy' f y (x:xs) = if (f x y) then (deleteBy' f y xs) else x:(deleteBy' f y xs) 

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]] 
groupBy' _ [] = []
groupBy' _ [x] = [[x]]
groupBy' f (x:xs) = let (y:ys) = (groupBy' f xs) in if (f x (head y)) then (x:y): ys else  [x] : (y:ys)

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f [] = []
concatMap' f (x:xs) =  (++) (f x) (concatMap' f xs)

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' f g x = if f x then x else until' f g (g x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x then x:(takeWhile' f xs) else  []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x then (dropWhile' f xs) else x:(dropWhile' f xs)

span', break' :: (a -> Bool) -> [a] -> ([a],[a])

span' f [] = ([],[])
span' f (x:xs) =  let (a,b) = (span' f xs) in if f x then (x:a,b)  else ([],x:xs)

break' f [] = ([],[])
break' f (x:xs) =  let (a,b) = (break' f xs) in if f x then ([],x:xs) else (x:a,b)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f (x:xs) (y:ys) = (f x y): (zipWith' f xs ys)

zipApply' :: [(a -> b)] -> [a] -> [b]
zipApply' [] [] = []
zipApply' (x:xs) (y:ys) = (x y): (zipApply' xs ys)

--
index :: [a] -> [(Int,a)]
index [] = []
index (x:xs) = (0,x): sumar1aLaLista (index xs)

sumar1aLaLista :: [(Int,a)] -> [(Int,a)]
sumar1aLaLista [] = []
sumar1aLaLista (x:xs) = sumarAlPar x : sumar1aLaLista xs

sumarAlPar :: (Int,a) -> (Int,a)
sumarAlPar (x,y) = (1+x,y) 

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f =  (.) f (applyN (n-1) f)

iterate' :: (a -> a) -> a -> [a]
iterate' f a = (iterate f a)


findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f [] = Nothing
findIndex' f (x:xs) = if (f x) then Just 0 else Just (sumar1Just (findIndex' f xs))

sumar1Just :: Maybe Int -> Int
sumar1Just (Just n) = 1 + n
sumar1Just Nothing = 0

-- Ejercicio 2.1

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

id' :: a -> a
id' = id'

const :: a -> b -> a
const x _ = x

fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b
snd' (_,y) = y

swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

head' :: [a] -> a
head' [] = error "No se Puede aplicar en lista vacia"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

sum', product' :: Num a => [a] -> a
sum' [] = 0
sum' x = (+) (head x) (sum'(tail x))

product' [] = 0
product' x = (*) (head x) (product'(tail x))


elem', notElem :: Eq a => a -> [a] -> Bool

elem' = (.) not . (.) isNothing . find' . (==)


notElem = (.) not . (.) isNothing . find' . (==)

and', or' :: [Bool] -> Bool
and' = all' id

or' = any' id

-- parcial en lista vacia
last' :: [a] -> a 
last' [a] = a
last' x = last' x

-- parcial en lista vacia
init' :: [a] -> [a]
init' [a] = []
init' x = (head' x) : init' (tail' x)

subset :: Eq a => [a] -> [a] -> Bool
subset = flip (all . flip elem)

(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) [] y = y
(+++) (x:xs) y =  x: ((+++) xs y) 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = (+++) x (concat' xs)

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 =  x
(!!!) (_:xs) n =  (!!!) xs (n-1)

take' :: Int -> [a] -> [a]
take' _ [] =  []
take' n (x:xs) = if n <= 0 then [] else x : take' (n-1) xs 

drop' :: Int -> [a] -> [a]
drop' _ [] =  []
drop' n (_:xs) = if n <= 0 then xs else  drop' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = ((take' n xs), (drop' n xs))

maximum', minimum' :: Ord a => [a] -> a
maximum' [a] = a
maximum' (x:xs) = if x > (maximum' xs) then x else (maximum' xs)

minimum' [a] = a
minimum' (x:xs) = if x < (minimum' xs) then x else (minimum' xs)

--lookup' :: Eq a => a -> [(a,b)] -> Maybe b
--lookup' k [] = Nothing
--lookup'  k ((k2, v) : kvs) = if k == k2 then Just v  else lookup'  k kvs

--lookup' k xs = (find' (esClave k ) xs)

--esClave :: Eq a => a -> (a,b) -> Bool
--esClave a (b,v2) = a==b





unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):xs) = let (list1, list2) = (unzip' xs) in (x:list1, y:list2) 

tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = ((x:xs):(tails xs)) ++ []

replicate' :: Int -> a -> [a]
replicate' n a = a : replicate (n-1) a 

repeat' :: a -> [a]
repeat' n = n:(repeat' n)

cycle' :: [a] -> [a]
cycle' xs = xs ++ (cycle' xs)

nats :: [Int]
nats = [1..]

agrupar' :: Eq a => [a] -> [[a]]
agrupar' = groupBy' (==)









 
