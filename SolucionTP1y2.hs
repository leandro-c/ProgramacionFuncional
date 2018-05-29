apply :: (a -> b) -> a -> b
apply f x = f x
-- apply f = f

-- apply :: (a -> b) -> (a -> b)
-- apply = id

twice :: (a -> a) -> a -> a
twice f = f . f

-- twice f x = f (f x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs
-- filter p (x:xs) = (if p x then (x:) else id) $ filter p xs

any, all :: (a -> Bool) -> [a] -> Bool
any p [] = []
any p (x:xs) = p x || any p xs

-- any p = or . map p

all p [] = []
all p (x:xs) = p x && all p xs

-- all p = and . map p

maybe :: b -> (a -> b) -> Maybe a -> b
maybe z f Nothing = z
maybe z f (Just x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy p [] = []
nubBy p (x:xs) = x : nubBy p (filter (not . p x) xs)

nub = nubBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy p x [] = []
deleteBy p x (y:ys) = if p x y then ys else x : deleteBy p x ys

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p [] = []
groupBy p [x] = [[x]]
groupBy p (x:xs) = let (ys:yss) = groupBy p xs
                       in if p x (head ys)
                       	     then (x:ys) : yss
                       	     else [x] : ys : yss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = f x ++ concatMap f xs

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x
	             then x
	             else until p f (f x)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) = if p x
                        then x : takeWhile p xs
                        else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) = if p x
                        then dropWhile p xs
                        else xs

span, break :: (a -> Bool) -> [a] -> ([a],[a])

span p xs = (takeWhile p xs, dropWhile p xs)

break p = span (not . p)
-- break = span . (.) not

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith xs ys

zipApply :: [(a -> b)] -> [a] -> [b]
zipApply = zipWith ($)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
-- iterate f x = map (\n -> applyN n f x) [0..]

-- applyN' n f x = iterate f x !! n

-- ejemplo de iterate
-- potenciasDe2 = iterate (*2) 0

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = maybe Nothing (Just . fst) . find (p . snd) . index

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f = map fst . filter (f . snd) . zip [0..]

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

fst :: (a,b) -> a
fst (x,y) = x
-- fst = uncurry const

snd :: (a,b) -> b
snd (x,y) = y
-- snd = uncurry (flip const)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
-- swap = uncurry (flip (,))

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

sum, product :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
-- length = sum . map (const 1)

elem :: Eq a => a -> [a] -> Bool
elem y [] = False
elem y (x:xs) = y == x || elem y xs

-- any . (==)

notElem :: Eq a => a -> [a] -> Bool
notElem y [] = True
notElem y (x:xs) = y /= x && notElem y xs

-- notElem x = not . elem x
-- notElem = (.) not . elem

and, or :: [Bool] -> Bool
and = all id

or = any id

last :: [a] -> a
last [x] = []
last (x:xs) = last xs
-- last = head . reverse

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
-- init = tail . reverse

subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys
-- subset xs = all (`elem` xs)
-- subset = flip (all . flip elem)

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

-- (++) [] = id
-- (++) (x:xs) = (x:) . (++) xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- concat = concatMap id

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) (n-1) xs

-- (!!) n = head . drop n
-- (!!) = (.) head . drop

take :: Int -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x:xs) = x : take xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop xs

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (,)

splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 (x:xs) = ([x], xs)
splitAt n [] = ([], [])
splitAt n (x:xs) = let (ys, zs) = splitAt (n-1) xs
                       in (x:ys, zs)

-- splitAt xs = (take n xs, drop n xs)

maximum, minimum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

minimum = negate . maximum . map negate

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup x [] = Nothing
lookup x ((k,v):kvs) = if x == k then Just v else lookup x kvs
-- lookup x = maybe Nothing (Just . snd) . find (\(k,v) -> x == k)

unzip :: [(a,b)] -> ([a],[b])
unzip xs = (map fst xs, map snd xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

-- tails = (++ [[]]) . takeWhile (not . null) . iterate tail

replicate :: Int -> a -> [a]
replicate n = take n . repeat
-- replicate n x = applyN n (x:) []

repeat :: a -> [a]
repeat = iterate id

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs
-- cycle xs = concat (repeat xs)

nats :: [Int]
nats = [1..]

intersperse :: a -> [a] -> [a]
intersperse x []  = []
intersperse x [y] = y
intersperse x (y:ys) = y : x : intersperse x ys

inits :: [a] -> [[a]]
inits = reverse . map reverse . tails . reverse

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] ys = True
isPrefixOf (x:xs) [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs = isPrefixOf xs . reverse

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex = findIndex . (==)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x ys = elemIndices' 0 x ys

elemIndices' n x [] = []
elemIndices' n x (y:ys) = if x == y then n : elemIndices' (n+1) x ys else elemIndices' (n+1) x ys
-- elemIndices' n x (y:ys) = (if x == y then (n :) else id) (elemIndices' (n+1) x ys)
-- elemIndices' n x (y:ys) = (if x == y then (n :) else id) $ elemIndices' (n+1) x ys

-- elemIndices x [] = []
-- elemIndices x (y:ys) =  (if x == y then (0 :) else id) $ map (+1) $ elemIndices' x ys

-- elemIndices x = map fst . filter ((==x) . snd) . zip [0..]

-- elemIndices = findIndices . (==) 

index :: [a] -> [(Int,a)]
index = zip [0..]

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x > y then y : x : ys else y : insert x ys

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

---------------------------------------------
--- más definiciones para entender ----------
---------------------------------------------

--- devuelve toda la lista de fibonacci
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib n = fibs !! n
-- fib = (!!) fibs

mapMax :: [(Int,Int)] -> [Int]
mapMax = map (uncurry max)

apariciones :: Eq a => a -> [a] -> Int
apariciones x = sum . map (\y -> if x == y then 1 else 0)

filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA x = filter (<x)
-- filter . flip (<)

mapLongitudes :: [[a]] -> [Int]
mapLongitudes = map length

longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA x = filter ((>x) . length)

snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x : snoc xs y

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos = zipWith max

zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort = zipWith (\x y -> (min x y, max x y))

factorial :: Int -> Int
factorial n = product [1..n]

splitMin :: Ord a => [a] -> (a, [a])
splitMin xs = let m = minimum xs in (m, delete m xs)

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = []
intersection (x:xs) ys = (if elem x ys then (x:) else id) $ intersection xs ys
-- filter . flip elem

union :: Eq a => [a] -> [a] -> [a]
union xs ys = nub (xs ++ ys)
-- union = (.) nub . (++)
