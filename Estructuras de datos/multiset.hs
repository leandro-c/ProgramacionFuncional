import Map
--Module--
data Multiset a = MkMs(Map a Int)

--crea un multiSet
--Inv. Rep.: Los valores no pueden ser negativos,osea son mayores a '0'
emptyMs::Multiset a --0(1)
emptyMs = MkMs(Map emptyM)

--Devuelve la cantidad de apariciones
--de un elemento
occursMs:: Ord a => a Multiset a -> Int --0(n)
occursMs n (MkMs(mapInt)) = devolverNumero (lookupM n mapInt)

devolverNumero:: Maybe a ->Int
devolverNumero Nothing = 0
devolverNumero Just a = a



--añade una aparicion de un elemento 
addMs::Ord a => a -> Multiset a -> Multiset a --0(n)
addMs n (MkMs mapInt ) = let ocurencia = lookupM n mapInt
							in agregarAMultiSet ocurencia mapInt

agregarAMultiSet:: a -> Map a Int ->Multiset a
agregarAMultiSet n mapInt = MkMs(assocM n devolverNumero((lookupM n mapInt)mapInt)+1 mapInt)
--decrementa la cantidad de apariciones de un elemento 
removeMs:: Ord a => a -> Multiset a -> Multiset a --0(n)
removeMs n MkMs( mapInt) = let ocurencia = lookupM n mapInt
								in quitarAMultiSet ocurencia mapInt

quitarAMultiSet::a -> Map a Int ->Multiset a						
quitarAMultiSet n mapInt = MkMs(deleteM n devolverNumero((lookupM n mapInt)mapInt)-1 mapInt))		

