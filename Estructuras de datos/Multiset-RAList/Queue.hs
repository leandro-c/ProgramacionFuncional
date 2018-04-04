module Queue(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = MkS [a]

emptyQ :: Queue a
emptyQ = MkS []

isEmptyQ :: Queue a -> Bool
isEmptyQ (MkS xs) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (MkS xs) = MkS (x:xs)

firstQ :: Queue a -> a
firstQ (MkS xs) = last xs

dequeue :: Queue a -> Queue a
dequeue (MkS xs) = MkS (init xs)