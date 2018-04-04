module Tribu
  (Tribu, 
  	esTribuVacia, 
  	agregar, 
  	esDeLaTribu,
  	desterrarJoven,
  	elMasJoven,
  	filtrarMenoresA) where

data Tribu = MkT (Heap Persona) (Set Persona)
-- Invariantes de Rep.
--- La Heap y el Set tiene que tener los mismos
---  elementos
--- y la misma cantidad de elementos.

esTribuVacia :: Tribu -> Bool
esTribuVacia (MkT h s) = isEmptyH h

agregar :: Persona -> Tribu -> Tribu -- O(log n)
agregar p (MkT h s) = 
	if belongs p s
	   then MkT h s
	   else MkT (insertH p h) (addS p s)

esDeLaTribu    :: Persona -> Tribu -> Bool  -- O(log n)
esDeLaTribu p (MkT h s) = belongs p s

desterrarJoven :: Tribu -> Tribu            -- O(log n)
desterrarJoven (MkT h s) = 
	MkT (deleteMinH h) (removeS (minH h) s) 

elMasJoven     :: Tribu -> Persona          -- O(1)
elMasJoven (MkT h s) = minH h

deMenorAMayor  :: Tribu -> [Persona]        -- O(n . log n)
deMenorAMayor (MkT h s) = heapToList h

-- O(n . lon g)
heapToList h = if isEmptyH h
	              then []
	              else minH h : heapToList (deleteMinH h)

-- Devuelve las personas mayores a cierto numero
losMayoresA    :: Int -> Tribu -> [Persona] -- O(n . log n)
losMayoresA n tribu = 
	filtrarMenoresA n (deMenorAMayor tribu)

filtrarMenoresA :: Int -> [Persona] -> [Persona]
filtrarMenoresA n []     = []
filtrarMenoresA n (x:xs) = 
	if edad x < n
	   then filtrarMenoresA n xs
	   else x:xs