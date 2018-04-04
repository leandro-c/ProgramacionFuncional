data Colores = Rojo | Negro | Azul 
data Persona = MkPersona Nombre Edad

type Nombre = String
type Edad = Int

-- *Funciones de construccino 
nacer:: Nombre->Persona
nacer n = MkPersona n 0 

-- *Funciones de acceso 
edad::Persona->Edad
edad (MkPersona _ e ) = e

nombre::Persona->Nombre
nombre(MkPersona n _) = n

crecer ::Persona->Persona
crecer (MkPersona n e) = MkPersona n (e+1) 

--precondicion : la lista no debe ser vacia.

elmasViejo::[Persona]->Persona
elmasViejo [p] =  p
elmasViejo (p:ps) = elMayor p (elmasViejo ps)


elMayor::Persona -> Persona -> Persona
elMayor p1 p2 = if (edad p1)>(edad p2)
					then p1
					else p2
					
--F::Eq a=>[a]->a
instance Eq Persona where
	p1==p2 = (nombre p1) == (nombre p2) && (edad p1) == (edad p2)
	p1 /= p2 = not (p1 == p2)


instance Ord Persona where
	p1 > p2 = 	(edad p1)>(edad p2)
	p1 < p2 = 	(edad p1)<(edad p2)				
	p1 >= p2 =	(edad p1)>=(edad p2)
	p1 <= p2 = 	(edad p1)<=(edad p2)


cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s p = MkPersona s (edad p) 


esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra p1 p2 = esMenor p1 p2

esMenor :: Ord a => a -> a -> Bool
esMenor x y = x < y
							

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if edad p > n 
						then p : mayoresA n ps
							else mayoresA n ps
sumatoria [Persona] -> Int
sumatoria [] = 0
sumatoria (x:xs) = 1+ sumatoria xs

promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad (p:ps) = div( (edad p)+promedio ps)  
