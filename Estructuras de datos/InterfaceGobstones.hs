import Map 
import Set

data Dir = Norte | Sur | Este | Oeste
data Color = Azul | Negro | Rojo | Verde
data Coordenada = MkCoord Int Int 
data Celda = MkCelda Int Int Int Int --Azul,Negro,Rojo,Verde
data Tablero = MkTablero(Map Coordenada Celda) Coordenanda

celdaActual::Tablero-> Celda
celdaActual (MkTablero mapc actual) =fromJust(lookUpM mapc actual) 

poner:: Color -> Tablero -> Tablero 
--proposito : dado un colos de tablero saca una  bolita del color especificado de la celda actual del tablero
--Inv:deber haber al menos una bolita del color especificado 
sacar::Color -> Tablero ->Tablero 
sacar color t@(MkTablero mapc actual) = 
	let celda = celdaActual t
	in
		if(hayBolitas color celda)
			then MkTablero(sacarEnMap color mapc celda actual)actual
			else error"BOOM"
sacarEnMap:: Color->Map Coordenada Celda->Celda->Map Coordenada Celda
sacarEnMap color mapc cell = assocM coord (sacarEnCelda color cell) mapc

sacarEnCelda::Color->Celda->Celda
sacarEnCelda Azul (MkCelda n _ _ _) = (MkCelda n-1 _ _ _) 
sacarEnCelda Negro(MkCelda _ n _ _) = (MkCelda _ n-1 _ _)
sacarEnCelda Rojo (MkCelda _ _ n _) = (MkCelda _ _ n-1 _)
sacarEnCelda Verde(MkCelda _ _ _ n) = (MkCelda : _ _ n-1)

hayBolitas::Color -> Celda -> Bool 
hayBolitas Azul (MkCelda n _ _ _) = n > 0
hayBolitas Negro(MkCelda _ n _ _) = n > 0
hayBolitas Rojo (MkCelda _ _ n _) = n > 0
hayBolitas Verde(MkCelda _ _ _ n) = n > 0


mover:: Dir -> Tablero -> Tablero 
mover  d (MkTablero mapc actual) = if(puedeMover d t)
				then (MkTablero mapc (cambiarLaCelda actual d)
				else error "Boom"

cambiarLaCelda::Coordenada -> Dir -> Coordenada 
cambiarLaCelda (MkCoord x y) Norte 	= x  y+1 
cambiarLaCelda (MkCoord x y) Sur 	= x y-1
cambiarLaCelda (MkCoord x y) Este 	= x-1 y
cambiarLaCelda (MkCoord x y) Oeste 	= x+1 y 

puedeMover:: Dir -> Tablero -> Bool 
puedeMover d (MkTablero mapc actual) = existeCoord mapc (cambiarLaCelda actual d)

existeCoord:: Map Coordenada Celda -> Coordenada -> Bool
existeCoord mapc coord = not isNothing(lookUpM coord mapc)

nuevoTablero::Int ->Int ->Tablero 
nuevoTablero  x y = (MkTablero (crearMapXY x y ) actual)

crearMapXY::Map Coordenada Celda ->Int ->Int -> Map Coordenada Celda
crearMapXY 0 y = crearFila 0 y
crearMapXY x y = unirMaps (crearFila x y)(crearMapXY (x-1) y)

crearFila::Int->Int->Map Coordenada Celda
crearFila x 0 = assocM emptyM (MkCoord x 0)nuevaCelda
crearFila x y = assocM (crearFila x (y -1)(MkCoord x y))nuevaCelda

nuevaCelda::Celda
nuevaCelda = MkCelda 0 0 0 0