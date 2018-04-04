module Map(Map, 
	emptyM, assocM, lookupM, removeM, domM) where

import Set

data Map k v = MkM [k] [v]

-- Inv. Rep.
-- Sea MKM ks vs
-- length ks == length vs

-- Observación:
-- Sea MKM ks vs
-- El elemento en la posicion i de ks es clave
-- del elemento en la posicion i de vs

-- O(1)
emptyM :: Map k v
emptyM = MkM [] []

-- O(n)
assocM :: Ord k => k -> v -> Map k v -> Map k v
assocM k v (MkM ks vs) = 
  let (ks2, vs2) = agregarAssoc k v ks vs
      in MkM ks2 vs2

  --let result = agregarAssoc k v ks vs
  --    in MkM (fst result) (snd result)

-- O(n)
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM k (MkM ks vs) = buscarPorClave k ks vs

-- O(n)
removeM :: Ord k -> k -> Map k v -> Map k v
removeM k (MkM ks vs) = 
  let (ks2, vs2) = borrarClave k ks vs
      in MkM ks2 vs2

-- O(1)
domM :: Ord k => Map k v -> [k]
domM (MkM ks _) = ks

------------------------------------------------
agregarAssoc :: Ord k => k -> v -> [k] -> [v] -> ([k], [v])
agregarAssoc k v []     []       = ([k], [v])
agregarAssoc k v (k2:ks) (v2:vs) = 
	if k == k2
	   then (k:ks, v:vs)
	   else agregarARec k v (agregarAssoc k v ks vs)

		--else let (ksrec, vsrec) = agregarAssoc k v ks vs
		--        in (k2:ksrec, v2:vsrec)

agregarARec :: Ord k => k -> v -> ([k], [v]) -> ([k], [v])
agregarARec k v (ks, vs) = (k:ks, v:vs)
    
buscarPorClave :: Ord k => k -> [k] -> [v] -> Maybe v
buscarPorClave k [] [] = Nothing
buscarPorClave k (k2:ks) (v2:vs) = 
  if k == k2
     then Just v2
     else buscarPorClave k ks vs

borrarClave :: Ord k => k -> [k] -> [v] -> ([k], [v])
borrarClave k [] [] = ([], [])
borrarClave k (k2: ks) (v2: vs) =
    if k == k2
        then (ks, vs)
        else let (ksrec, vsrec) = borrarClave k ks vs
             in (k2:ksrec, v2:vsrec)




