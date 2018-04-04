module ColasYPilas(Conjunto,
agregarC,
vacioC,
perteneceC,
cantidadC,
borrarC,
unionC,
listaC)where

data conjunto = Conjunto [a]
--Crea un conjunto vacío.
agregarC :: Eq a => a -> Conjunto a -> Conjunto a
agregarC a c = Conjunto []

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
vacioC :: Conjunto a




--Dados un elemento y un conjunto indica si el elemento pertenece al con-
--junto.
perteneceC :: Eq a => a -> Conjunto a -> Bool

--Devuelve la cantidad de elementos distintos de un conjunto
cantidadC :: Eq a => Conjunto a -> Int


--Devuelve la cantidad de elementos distintos de un conjunto
borrarC :: Eq a => a -> Conjunto a -> Conjunto a



--Dados dos conjuntos devuelve un conjunto con todos los elementos de
--ambos conjuntos.
unionC :: Eq a => Conjunto a -> Conjunto a -> Conjunto a



--Dado un conjunto devuelve una lista con todos los elementos distintos del
--conjunto.
listaC :: Eq a => Conjunto a -> [a]
