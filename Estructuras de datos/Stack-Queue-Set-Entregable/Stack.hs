module Stack(Stack, emptyS, isEmptyS, push, top, pop) where

data Stack a = MkS [a]

emptyS :: Stack a
emptyS = MkS []

isEmptyS :: Stack a -> Bool
isEmptyS (MkS xs) = null xs

push :: a -> Stack a -> Stack a
push x (MkS xs) = MkS (x:xs)

top :: Stack a -> a
top (MkS xs) = head xs

pop :: Stack a -> Stack a
pop (MkS xs) = MkS (tail xs)