module Persona(
	Persona,
	crearP, 
	edad, 
	nombre, 
	decrecer) where

data Persona = MkP String Int

-- Invariantes de Representación
--- Si tengo MkP n e
----- entonces n no es vacio y e >= 0

-- Prec.: n y e no pueden ser vacío
crearP :: String -> Int -> Persona
crearP n e = 
	if null n || e < 0
		then error "Persona incorrecta"
        else MkP n e

edad :: Persona -> Int
edad (MkP n e) = e

nombre :: Persona -> String
nombre (MkP n e) = n

decrecer :: Persona -> Persona
decrecer (MkP n e) = 
	crearP n (e-1)

	--if e == 0
	--  then error "Una persona no puede tener edad negativa"
	--  else MkP n (e-1)