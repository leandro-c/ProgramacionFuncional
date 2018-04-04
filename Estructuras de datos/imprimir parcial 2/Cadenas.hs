module Cadenas (...)where


	data Cadenas MkC (Tree Bool)

	--inv.Rep.:Las hojas del arbol siempre son True

--son todas totales c= longitud de cadena binaria

emptyC ::Cadenas --0(1)
emptyC = MkC EmptyT
 
addC::String->Cadenas->Cadenas --0(c)
addC s (MkC t) = insertT s t

removeC::String -> Cadenas -> Cadenas --0(c)
removeC s (MkC t) = MkC deleteT t

belongsC::String -> Cadenas -> Bool --0(c)
belongsC s (MkC t) = recorrerEnArbol s t

recorrerEnArbol:: String -> Tree Bool
recorrerEnArbol [] EmptyT = Node True EmptyT EmptyT
recorrerEnArbol [] (Node x t1 t2) = Node

lasQueEmpiezanCon::String->Cadenas->[Cadenas] --0(c)

toListC::Cadenas->[String] 0(c)



---usuario

addCT::String-> Tree Bool -> Tree Bool

borrarCT:: String-> Tree Bool -> Tree Bool

chequearNodo::Tree Bool -> Tee Bool