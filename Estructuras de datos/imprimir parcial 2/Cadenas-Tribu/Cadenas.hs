module Cadenas(Cadenas, 
	emptyC, 
	addC, 
	removeC, 
	belongsC, 
	lasQueEmpiezanCon, 
	toListC) where

data Cadenas = MkC (Tree Bool)
-- Inv. Rep.
--- + Las hojas del arbol siempre son True

-- Son todas totales
-- c = longitud de cadena binaria
emptyC   :: Cadenas -- O(1)
emptyC   = MkC EmptT

addC     :: String -> Cadenas -> Cadenas -- O(c)
addC s (MkC t) = MkC (addCT s t)

addCT :: String -> Tree Bool -> Tree Bool
addCT [] EmptyT          = NodeT True EmptyT EmptyT
addCT [] (NodeT b t1 t2) = NodeT True t1 t2
addCT (x:xs) EmptyT      =
	if x == '0'
	   then NodeT False (addCT xs EmptyT) EmptyT
	   else NodeT False EmptyT (addCT xs EmptyT)
addCT (x:xs) (NodeT b t1 t2) =
	if x == '0'
	   then NodeT b (addCT xs t1) t2
	   else NodeT b t1 (addCT xs t2)

removeC  :: String -> Cadenas -> Cadenas -- O(c)
removeC s (MkC t) = MkC (borrarCT s t)

borrarCT :: String -> Tree Bool -> Tree Bool
borrarCT [] EmptyT          = EmptyT
borrarCT [] (NodeT b t1 t2) = chequearNodo (NodeT False t1 t2)
borrarCT (x:xs) EmptyT      = EmptyT
borrarCT (x:xs) (NodeT b t1 t2) = 
	if x == '0'
	   then chequearNodo (NodeT b (borrarCT xs t1) t2)
	   else chequearNodo (NodeT b t1 ((borrarCT xs t2))

chequearNodo :: Tree Bool -> Tree Bool
chequearNodo (NodeT False EmptyT EmptyT) = EmptyT
chequearNodo t = t

belongsC :: String -> Cadenas -> Bool    -- O(c)
belongsC s (MkC t) = 
	buscarCadena s t


buscarCadena _  EmptyT          = False
buscarCadena [] (NodeT b t1 t2) = b
buscarCadena (x:xs) (NodeT b t1 t2) = 
	if x == '0'
	   then buscarCadena xs t1
	   else buscarCadena xs t2

lasQueEmpiezanCon :: String -> Cadenas -> [String] -- O(n)
lasQueEmpiezanCon s (MkC t) = 
	empiezanConT s t s

empiezanConT []      EmptyT          s = []
empiezanConT (x:xs)  EmptyT          s = []
empiezanConT []      (NodeT b t1 t2) s =
	devolverTodas (NodeT b t1 t2) s

empiezanConT (x:xs) (NodeT b t1 t2) s = 
	if x == '0'
	   then empiezanConT xs t1 s
	   else empiezanConT xs t2 s

devolverTodas [] EmptyT s = [] 
devolverTodas (x:xs) (NodeT b t1 t2) s = 
	if b
	   then s :
	       (devolverTodas t1 (snoc '0' s) ++ 
	       	devolverTodas t2 (snoc '1' s))
	   else 
	   	    devolverTodas t1 (snoc '0' s) ++ 
	       	devolverTodas t2 (snoc '1' s)

toListC  :: Cadenas -> [String] -- O(n)
toListC cs = lasQueEmpiezanCon "" cs