import Maybe
import Map

-- Proposito: dada una lista
-- devuelve una lista de listas
-- en donde cada sublista es
-- la agrupacion de elementos iguales consecutivos

-- Ejemplo: [1,1,1,2,2,3,3,3,1] -> [[1,1,1], [2,2], [3,3,3], [1]]
agrupar :: Eq a => [a] -> [[a]]
agrupar []     = []
agrupar (x:[]) = [x] : [] 
agrupar (x:xs) =  
   let (xsrec:xss) = agrupar xs
      in if x == head xsrec
            then ((x:xsrec) : xss)
            else [x] : xsrec : xss
-- Variante con subtarea
--   agruparARec x (agrupar xs)

-- Def. de subtarea
--agruparARec x (xs:xss) = 
--    if x == head xs
--       then ((x:xs) : xss)
--       else [x] : xs : xss

-- Proposito: dada una lista de pares
-- devuelve un par de listas, donde la primera
-- lista esta formada con las primeras componetes
-- de cada par, y la segunda lista con las
-- segundas componentes
concaternarPares :: [(a, a)] -> ([a], [a])
concaternarPares [] = ([], [])
concaternarPares ((x,y):ps) = 
	let (fsts, snds) = concaternarPares ps
        in (x:fsts, y:snds)

-- concatenarPares [(1,2), (3,4), (5,6)] -> ([1,3,5], (2,4,6))

-- La lista no es vacia
-- Proposito: dada una lista devuelve el minimo
-- y una lista sin ese minimo
splitMin :: Ord a => [a] -> (a, [a])
splitMin [x] = (x, [])
splitMin (x:xs) = 
	let (m, sinMin) = splitMin xs
	    in (min x m, max x m : sinMin)

-- pueden estar en otro orden
-- [3,1,2,5,1] -> (1, [3,2,5,1])

-- Proposito:
-- Dada una lista de listas
-- devuelve el minimo de cada sublista.
-- Precondicon: ninguna sublista es vacia
-- Observacion: resolver usando splitMin, 
minimos :: Eq a => [[a]] -> [a]
minimos [] = []
minimos (xs:xss) = 
	let (m, _) = splitMin xs
	    in m : minimos xss

-- Prec.: las claves de [k] existen Map k v
-- usar let para obtener el resultado del lookup
obtenerClaves :: Ord k => [k] -> Map k v -> [v]
obtenerClaves [] m = []
obtenerClaves (x:xs) m = 
-- sin let
   fromJust (lookupM x m) : obtenerClaves xs

-- con let
 --let (Just v) = lookupM x m
 --    in v : obtenerClaves xs m