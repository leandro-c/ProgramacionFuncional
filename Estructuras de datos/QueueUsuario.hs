--dice el largo de la stack
lenS::Stack a -> Int
lenS stack = if isEmptyS stack
				then 0
				else 1 + lenS(pop(stack))


--Devuelve el ultimo elemento de una stack 
--parcial en listavacia
lastS::Stack a -> a
lastS stack = if isEmptyS stack
				then  pop stack 
					else lastS(pop stack)

--Pasa una stack a la lista 
stackToList::Stack a -> [a]
stackToList stack = if isEmptyS stack
						then []
							else top stack : stackToList (pop stack)

--Parcial nota calculo
--20% nota1 +80% nota2

--0(n.n)= 0(n^2)
--porque firstQ y dequeue son 0(n)
--y recorro cada elemento de la queue 
queueToList:: Queue a -> [a]
queueToList q = if isEmptyQ q
				then []
				else firstQ q : queueToList(dequeue q)
--0(n^2)
listToQueue::[a]->Queue a
listToQueue [] = emptyQ
listToQueue x:xs = queue x (listToQueue xs)

--parcial en cola vacia
--0(n^2)
lastQ::Queue a -> a
lastQ q = if isEmptyQ dequeue q
			then firstQ q 
				else lastQ(dequeue q)

--parcial en cola vacia 
initQ:: Queue a -> Queue a
initQ q = if isEmptyQ dequeue q 
			then emptyQ
			else queue (firstQ q) initQ(dequeue q)


balanceado :: String -> Bool
--"()" -> true
--"(())" ->true
--"(()" -> false
--"())" -> false
--"()()"-> true 
---")(-2 -> false
balanceado s = chequearParens s emptyS


chequearParens::String -> Stack Char -> Bool 
chequearParens [] s = undefined...
chequearParens ('(':xs)s = undefined...
chequearParens (')':xs)s= undefined...
chequearParens (x:xs) s= undefined...
