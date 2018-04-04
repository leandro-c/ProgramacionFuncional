module Queue (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)where
--Invariante de Rep.:
---si la lista de dequeue es vacia entonces la cola esta vacia 
----la primer lista debe tener elementos si la segunda esta vacia
-----es error MkQueue [] [1,2] 2

data Queue a = MkQueue [a] [a] Int

laCola::Queue Int
laCola = (queue 5(queue 2 (queue 1 emptyQ )))

--laCola = MkQueue [1,2,3] 3

--Crea una cola vacía.
emptyQ::Queue a
emptyQ = (MkQueue [] [] 0)


--Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (MkQueue [] _ _) = True
isEmptyQ (MkQueue [a] _ _ )= False


--Dados un elemento y una cola, agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a
queue x (MkQueue [] ys size) = MkQueue (x:[]) ys (size +1)
queue x (MkQueue xs ys size) = MkQueue (xs)(x:ys)(size +1)

snoc :: [a] ->  a -> [a]
snoc [] n = [n]
snoc (x:xs) n = x : (snoc xs n)

--Dada una cola devuelve el primer elemento de la cola.
--precondicion que first
firstQ :: Queue a -> a
firstQ (MkQueue xs _ _) = head xs

--Dada una cola la devuelve sin su primer elemento.
--La queue no tiene que ser vacia.
dequeue :: Queue a -> Queue a
dequeue (MkQueue xs ys size) = if(length xs == 1)
then MkQueue  (reverse ys) [] (size -1) 
else MkQueue (tail xs)(ys) (size -1)

--reverse ::[a] -> [a]
--reverse [] = []
--reverse	(x:xs) = snoc (reversa xs) x

lenQ:: Queue a -> Int
lenQ (MkQueue _ _ size) = size

module Stack (Stack,emptyS,isEmptys,push,top,pop)where

data Stack a = Mks [a] Int[a]

--Inv de Rep.:
---Siendo Mks xs n hm entonces 
----length xs ==n

--para todo i, indice de la lista pasa que el maximo a partir

emptyS::Stack a 
emptyS = Mks [] 0 [] 

isEmptyS::Stack a -> Bool
isEmptyS (Mks xs n hm) = n == 0
