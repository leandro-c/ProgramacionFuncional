id'::a -> a
id' a = a

const':: a -> b -> a
const' a _ = a

fst':: (a,b)->a
fst' (a, _ ) = a

snd':: (a,b)->b
snd' (_,b) = b

swap':: (a,b) -> (b,a)
swap' (a,b) = (b,a)

sum':: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


elem':: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = a==x || elem' a xs

notElem':: Eq a => a -> [a] -> Bool
notElem' x [] = True
notElem' y (x:xs) = x /= y || elem' y xs

and':: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || and' xs

last':: [a] -> a
last' [] = error "no podes pedir algo que no hay" 
last' [x] = x
last' ( _ :xs) = last xs

init' :: [a] -> [a]
init' [a] = []
init' (x:xs) = x : init' xs

subset' :: Eq a => [a] -> [a] -> Bool
subset' []  []= True
--subset' xs [] = False 
subset' [] ys = True    
subset' (x:xs) (y:ys) =  elem x (y:ys) && subset' xs ys

(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
--(+++) x [] = x
(+++) [] y = y
(+++) (x:xs) ys = x:(+++)xs ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

(!!!) :: [a] -> Int -> a
(!!!) [] n = error "estas pidiendo indice de una lista vacia"
(!!!) (x:_) 0 = x
(!!!) (xs) n = (!!!) xs (n-1)

take' :: Int -> [a] -> [a]
take' n [] = error "no podes tomar nada de una lista vacia"
take' 1 (x:xs) = [x]
take' n (x:xs) = if n <= 0 then [] else x : take' (n-1) xs 
--take' n (x:xs) = x: take' (n-1)xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (x:xs) =  if n <= 0 
                    then xs
                     else  drop' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
--zip' xs [] = []
zip' [] xs = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)
--
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = ((take' n xs), (drop' n xs))

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
tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:xs) = ((x:xs):(tails' xs)) ++ []

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

--2. Tipo a expresiones
--1. Dar tipo a las siguientes expresiones y funciones
--a) 
--True::Bool
--b) 
--[2]::[Int]
--c) 
--Maybe ["Jorge"]::(a -> [[Char]]) -> Maybe a -> [[Char]])
--d) 
--Nothing::Maybe a
--e) 
--[]::[a]
--f ) 
--let x = [] in x ++ x::[a]
--g) 
--let f x = f x in f []::a
--h) 
--data Either a b = Left a | Right b
--            x = Left True
--            y = Right (Left [])
--            z = Right (Left [Right []])
--i) 
--(:):: a -> [a] -> [a]
--j) 
--Maybe::a -> (b -> a) -> Maybe b -> a
--k) 
--Right::a -> Either b a
--l) 
--(1:)::Num a => [a] -> [a]
--m) 
--error "ups":: a
--n) 
--error:: String -> a
--ñ) 
--undefined::a

--o) undefined undefined
--2. Dar ejemplos de expresiones que posean los siguientes tipos:
--a)
--Bool
-- True && False && False
--b) 
--(Int, Int)::(Num a, Num b) => (b,a)
--(6,6)
--c) 
--Int -> Int -> Int:Int
--armarLista::Int -> Int -> Int:Int
--d) 
--a -> a
--id::a->a
--e) 
--a
--undefined
--f) 
--String -> a
--g) 
--a -> b
-- 4. Patterns

-- 1. (x, y)  Verdadero

-- 2. (1, y)  Verdadero
-- 3. (n+1) Falso
-- 4. (’a’,(’a’,b)) Verdadero
-- 5. (a,(a,b)) Verdadero
-- 6. ([]:[4]) }Verdadero
-- 7. (x:y:[]) Verdadero
-- 8. [x] Verdadero
-- 9. ([]:[]) Verdadero

--5. Terminación
--Indicar qué programas terminan
--1. let nats = [1..] in nats no termina
--2. take 5 [1..] Termina
--3. let appendedNats = [1..] ++ [1..] in take 5 appendedNats Termina
--4. let x = x in x 	No Termina
--5. undefined 
--6. undefined undefined

--6.
--6.1
--Enum -- que pueden ser enumerados 
--Ord --is for types that have an ordering
--Eq --is used for types that support equality testing
--Bounded --members have an upper and a lower bound.
--Show --Members of Show can be presented as strings
--Read --Read is sort of the opposite typeclass of Show.
--Num -- is a numeric typeclass.
