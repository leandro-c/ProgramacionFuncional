data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

data Dir = Izq | Der

data Celda = ConsCelda Int Int

ej::Arbol Celda
ej =(Nodo (ConsCelda 3 1) (Nodo (ConsCelda 4 1) Vacio Vacio) (Nodo (ConsCelda 6 1) (Nodo (ConsCelda 5 7) Vacio Vacio) Vacio))

isEmptyT:: Arbol Celda -> Bool
isEmptyT Vacio = True
isEmptyT(Nodo x t1 t2) = False

--isDirDer::Dir -> Bool 
--isDirDer Izq = False 
--isDirDer Der = True


instance Eq Dir where
	Der == Der = True
	Izq == Izq = True
	_ == _ = False
	
--Dado un camino y un  ́arbol, indica si ese camino existe en el  ́arbol. Un camino existe si puedo completarlo sin
--que se termine el  ́arbol.
--Chequear en donde tengo que hacer la recursion.
existeCamino :: [Dir] -> Arbol Celda -> Bool
existeCamino [] Vacio = False
existeCamino [] (Nodo c _ _) = True
existeCamino ds Vacio = False		
existeCamino (d:ds) (Nodo c t1 t2) =  if d == Izq
									  then (existeCamino ds t1) 
									  else (existeCamino ds t2)


--Dado un camino y un arbol, retorna la cantidad de bolitas rojas que posee la celda al final del camino.
rojasDeCelda :: [Dir] -> Arbol Celda -> Int
rojasDeCelda _ Vacio = 0
rojasDeCelda [] (Nodo c t1 t2) = (cantidadRojas c)
rojasDeCelda (d:ds) (Nodo c t1 t2) = if d == Izq
						then  (rojasDeCelda ds t1)
						else  (rojasDeCelda ds t2)


cantidadRojas::Celda->Int 
cantidadRojas (ConsCelda _ n) = n


--Dado un  ́arbol de celdas retorna la celda que tenga m ́as bolitas rojas.
---parcial en arbol vacio , no pongan un arbol vacio 
celdaConMasRojas :: Arbol Celda -> Celda
celdaConMasRojas (Nodo c Vacio Vacio) = c 
celdaConMasRojas (Nodo c t1 Vacio) = maximo c (celdaConMasRojas t1)
celdaConMasRojas (Nodo c Vacio t2) = maximo c (celdaConMasRojas t2)
celdaConMasRojas (Nodo c t1 t2) = if (cantidadRojas c >cantidadRojas((maximo(celdaConMasRojas t1)(celdaConMasRojas t2))))
									then c
									else maximo(celdaConMasRojas t1)(celdaConMasRojas t2)
									
maximo:: Celda -> Celda -> Celda
maximo c1 c2 = if (cantidadRojas c1 > cantidadRojas c2)
				then c1
				else c2

--Dada una lista de caminos vacıa las celdas que se encuentren al final de dichos caminos.
vaciarCeldas :: [[Dir]] -> Arbol Celda -> Arbol Celda
vaciarCeldas [] Vacio = Vacio
vaciarCeldas [] t = t
vaciarCeldas (ds:dss) t = (vaciarCamino ds (vaciarCeldas dss t))

vaciarCamino::[Dir]->Arbol Celda->Arbol Celda
vaciarCamino _ Vacio = Vacio
vaciarCamino [] (Nodo c t1 Vacio) = (Nodo (vaciarCelda c) t1 Vacio) 
vaciarCamino [] (Nodo c Vacio t2) = (Nodo (vaciarCelda c) Vacio t2)
vaciarCamino [] (Nodo c Vacio Vacio) = (Nodo (vaciarCelda c) Vacio Vacio)
vaciarCamino [] (Nodo c t1 t2)= (Nodo (vaciarCelda c) t1 t2)
vaciarCamino (d:ds) (Nodo c t1 t2) = if (d == Izq)
										then (Nodo  c (vaciarCamino ds t1) t2) 
										else (Nodo c t1 (vaciarCamino ds t2))

vaciarCelda::Celda->Celda
vaciarCelda (ConsCelda x y) = (ConsCelda 0 0) 


--Dado un  ́arbol retorna el camino mas largo desde la raız hasta alguna de sus hojas, o, en otras palabras,
--el camino desde la raiz hasta la hoja m ́as lejana.
--caminoMasLargo :: Arbol Celda -> [Dir]
--caminoMasLargo Vacio = []
--caminoMasLargo (Nodo x Vacio Vacio) = []
--caminoMasLargo (Nodo c t1 t2) = quedarmeConLaMasGrande(agregarA(caminoMasLargo t1 ++ caminoMasLargo t2))

--agregarA:: a ->[[a]]->[[a]]
--agregarA y [] = []
--agregarA y (xs: xss) = (y:xs):agregarA y xss

--quedarmeConLaMasGrande::[[a]]->[a]
--quedarmeConLaMasGrande [] = []
--quedarmeConLaMasGrande (xs:xss) = if (length xs > quedarmeConLaMasGrande xss)
--then xs
--else quedarmeConLaMasGrande xss


totalCelda::[Dir]->Arbol Celda->Int
totalCelda _ Vacio = 0
totalCelda	[] (Nodo c t1 t2) = (totalEnNodo c)
totalCelda (d:ds) (Nodo c t1 t2) = if(d == Izq)
then totalCelda ds t1
else totalCelda ds t2

totalEnNodo::Celda->Int 
totalEnNodo (ConsCelda x y) = x+y
