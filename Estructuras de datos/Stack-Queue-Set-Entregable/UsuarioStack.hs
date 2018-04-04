import Stack
import Queue

-- O(n)
lenS :: Stack a -> Int
lenS s = if isEmptyS s
	        then 0
	        else 1 + lenS (pop s)

-- O(n)
-- Parcial en emptyS
lastS :: Stack a -> a
lastS s = if isEmptyS (pop s)
	         then top s
	         else lastS (pop s)

-- O(n)
stackToList :: Stack a -> [a]
stackToList s = 
	if isEmptyS s
       then []
       else top s : stackToList (pop s)


-- O(n . n) = O(n^2)
-- O(n^2) porque firstQ y dequeue cuestan O(n)
queueToList :: Queue a -> [a]
queueToList q = 
	if isEmptyQ q
	   then []
	   else firstQ q : queueToList (dequeue q)

-- O(n) si firstQ y dequeue cuestan O(1)
 
list2QOtra :: [a] -> Queue a
list2QOtra xs = list2QAlRevez (reverse xs)

-- O(n)
-- porque enqueue cuesta O(1)
list2QAlRevez :: [a] -> Queue a
list2QAlRevez [] = emptyQ
list2QAlRevez (x:xs) = 
	enqueue x (list2QAlRevez xs)

-- O(n . n) = O(n^2)
list2Q :: [a] -> Queue a
list2Q xs = 
	if null xs
	  then emptyQ
      else enqueue (last xs) (list2Q (init xs))


-- O(n^2)
reverseQ :: Queue a -> Queue a
reverseQ q = list2QAlRevez (queueToList q)