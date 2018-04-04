module RAList(RAList, emptyRAL, addRAL, removeRAL, get, set, sizeRAL) where

import Map
import Maybe

data RAList a = MkRAL (Map Int a) Int

-- Inv. Rep.
-- Dado MkRAL m n
-- "n" es la cantidad de elementos "m"
-- "m" posee como claves todos los numeros
-- entre 0 y (n-1)

emptyRAL :: RAList a
emptyRAL = MkRAL emptyM 0

-- Añade un elemento al final
-- de la lista
addRAL :: a -> RAList a -> RAList a
addRAL x (MkRAL m n) = MkRAL (assocM n x m) (n+1)

-- Borra el elemento al final
-- de la lista
-- Precondicion: la lista no puede ser vacia
removeRAL :: RAList a -> RAList a
removeRAL (MkRAL m n) = MkRAL (removeM (n-1) m) (n-1)

-- Devuelve un elemento en determinado índice
-- Precondicion: el índice existe en la lista
get :: Int -> RAList a -> a
get i (MkRAL m n) =
	if i < 0
	   then error "El indice no puede ser negativo"
       else if i >= n
	        then error "El indice no puede sobrepasar el tamaño de la lista"
	        else fromJust (lookupM i m)

-- Reemplaza un elemento de determinado índice
-- Precondicion: el índice existe en la lista
set :: Int -> a -> RAList a -> RAList a
set i x (MkRAL m n) = 
	if i < 0
	   then error "El indice no puede ser negativo"
	   else if i >= n
	           then error "El indice no puede sobrepasar el tamaño de la lista"
	           else MkRAL (assocM i x m) n

-- Devuelve la cantidad de elementos de
-- la lista
sizeRAL :: RAList a -> Int
sizeRAL (MkRAL m n) = n

--- TAREA

appendRAL :: RAList a -> RAList a -> RAList a
appendRAL (MkRAL m1 n1) (MkRAL m2 n2) = undefined