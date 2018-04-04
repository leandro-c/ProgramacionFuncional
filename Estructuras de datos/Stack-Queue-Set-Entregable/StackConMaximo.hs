module StackConMaximo(Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = MkS [a] Int [a]

-- Inv. Rep.
-- Siendo MkS xs n hm, entonces 
-- + - n == length xs
-- + - length xs == length hm
-- + - para todo "i", si saco "i" elementos
-- + de xs e "i" elementos
-- + de hm, entonces max xs == head hm

emptyS :: Stack a
emptyS = MkS [] 0 []

isEmptyS :: Stack a -> Bool
isEmptyS (MkS xs n hm) = n == 0

push :: Ord a => a -> Stack a -> Stack a
push x (MkS xs n hm) = 
	MkS (x:xs) (n+1) (actualizarMax x hm)

actualizarMax :: Ord a => a -> [a] -> [a]
actualizarMax x []     = [x] 
actualizarMax x (m:ms) = max x m : m : ms

top :: Stack a -> a
top (MkS xs _ _) = head xs

pop :: Stack a -> Stack a
pop (MkS xs n hm) = MkS (tail xs) (n-1) (tail hm)

-- O(1)
lenS :: Stack a -> Int
lenS (MkS _ n _) = n

-- O(1)
maxS :: Ord a => Stack a -> a
maxS (MkS _ _ hm) = head hm

--lenS s = if isEmptyS s
--	        then 0
--	        else 1 + lenS (pop s)