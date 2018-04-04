import Color
import Persona

-- import Lista

-- import ListaTree

import ListaAppend

crecer :: Persona -> Persona
crecer p = crearP (nombre p) (edad p + 1)

sumatoria :: Lista Int -> Int
sumatoria xs = 
	if isEmptyL xs
	   then 0
	   else headL xs + sumatoria (tailL xs)

-- Prec.: lista no vacia
minimoL :: Lista Int -> Int
minimoL xs = 
	if isEmptyL (tailL xs)
		then headL xs
		else min (headL xs) (minimoL (tailL xs))

ejemplo :: Lista Int
ejemplo = consL 1 (consL 2 (consL 3 emptyL))