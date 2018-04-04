module Lista(Lista,
	emptyL, 
	isEmptyL,
	headL,
	tailL,
	consL,
	appendL,
	lengthL) where

--             Funcion de Abstracción
data Lista a = MkL [a]

emptyL :: Lista a
emptyL = MkL []

isEmptyL :: Lista a -> Bool
isEmptyL (MkL xs) = null xs

headL :: Lista a -> a
headL (MkL xs) = head xs

consL :: a -> Lista a -> Lista a
consL x (MkL xs) = MkL (x:xs)

tailL :: Lista a -> Lista a
tailL (MkL xs) = MkL (tail xs)

appendL :: Lista a -> Lista a -> Lista a
appendL (MkL xs) (MkL ys) = MkL (xs ++ ys)

lengthL :: Lista a -> Int
lengthL (MkL xs) = lengthRep xs

lengthRep :: [a] -> Int
lengthRep [] = 0
lengthRep (x:xs) = 1 + lengthRep xs