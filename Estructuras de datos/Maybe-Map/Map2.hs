module Map(Map, 
	emptyM, assocM, lookupM, removeM, domM) where

import Set

data Map k v = MkM [(k, v)]

-- Observación (no es un invariante)
-- Las claves se pueden repetir

-- O(1)
emptyM :: Map k v
emptyM = MkM []

-- O(1)
assocM :: Ord k => k -> v -> Map k v -> Map k v
assocM k v (MkM kvs) = MkM (agregarAssoc k v kvs) 

-- O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM k (MkM kvs) = buscarPorClave k kvs

-- O(n)
removeM :: Ord k => k -> Map k v -> Map k v
removeM k (MkM kvs) = MkM (borrarClave k kvs)

-- O(n)
domM :: Ord k => Map k v -> Set k
domM (MkM kvs) = devolverClaves kvs

------------------------------------------------

-- O(1)
agregarAssoc :: Ord k => k -> v -> [(k, v)] -> [(k, v)]
agregarAssoc k v kvs =
	(k, v) : kvs
    
-- O(n)
buscarPorClave :: Ord k => k -> [(k,v)] -> Maybe v
buscarPorClave k [] = Nothing
buscarPorClave k ((k2, v) : kvs) =
    if k == k2
       then Just v
       else buscarPorClave k kvs

-- O(n)
borrarClave :: Ord k => k -> [(k, v)] -> [(k, v)]
borrarClave k [] = []
borrarClave k ((k2, v) : kvs) =
    if k == k2
        then borrarClave k kvs
        else (k2, v) : borrarClave k kvs

-- O(n)
-- Suponemos que "add" nos cuesta O(1)
devolverClaves :: Ord k => [(k, v)] -> Set k
devolverClaves [] = emptySet
devolverClaves ((k, _) : kvs) = 
    add k (devolverClaves kvs)