data TipoDePokemon = Agua | Planta | Fuego 
data Pokemon = MkPokemon Nombre TipoDePokemon PorcentajeEnergia
data Entrenador = MKEntrenador Nombre [Pokemon]

type Nombre = String
type Agua = String
type Fuego = String 
type Planta = String
type PorcentajeEnergia = Int


instance Eq TipoDePokemon where 
	Agua == Agua = True
	Fuego == Fuego = True
	Planta == Planta = True
	_ = _ = False 

-- *Funciones de acceso 
tipo::Pokemon->TipoDePokemon
tipo (MkPokemon _ t _ ) = t

energia::Pokemon->TipoDeEnergia
energia (MkPokemon _ _ e) = e

pokemones::Entrenador->[Pokemon]
pokemones (MkEntrenador _ []) = []
pokemones (MkEntrenador _ ps) = ps
--
elementoGanador :: TipoDePokemon -> TipoDePokemon
elementoGanador Agua = Planta  
elementoGanador Fuego = Agua
elementoGanador Planta = Fuego

leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA p1 p2 = tipo p1 == elementoGanador(tipo p2)

 
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon p (MkEntrenador nomb ps) = MkEntrenador nomb (cons p ps)

cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons (MkEntrenador _ ps) = length xs

cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo  t (MkEntrenador _ (p:ps)) = if (t == tipo p)
							then 1 + cantidadDePokemonsDeTipo t ps
							else cantidadDePokemonsDeTipo t ps
																		
lePuedeGanar :: Entrenador -> Pokemon -> Bool
lePuedeGanar (MkEntrenador _ ps) p2 = algunoLeGana p2 ps

algunoLeGana::[Pokemon]->Pokemon->Bool
algunoLeGana [] p = False
algunLeGana (x:xs) p = leGanaA x p || (algunoLeGana xs p)


puedenPelear :: TipoDePokemon -> Entrenador -> Entrenador -> Bool
puedenPelear t e1 e2 = puedePelear t e && puedePelear t e2





puedePelear:: TipoDePokemon -> Entrenador -> Bool
puedePelear t (MkEntrenador _ []) = False
puedePelear t (MkEntrenador _ (p:ps)) = pelearPuede t p && (puedePelear t ps ) 

pelearPuede:: TipoDePokemon -> Pokemon ->Bool
pelearPuede t p = esDelTipo t p && tieneEnergia p

esDelTipo::TipoDePokemon -> Pokemon -> Bool
esDelTipo Planta (MkPokemon _ Planta _)= True
esDelTipo Agua (MkPokemon _ Agua _)=True
esDelTipo Fuego (MkPokemon _ Fuego _)= True
esDelTipo _ _ = False

tieneEnergia::Pokemon -> Bool
tieneEnergia (MkPokemon _ _ e)= (e > 0)




esExperto :: Entrenador -> Bool
esExperto MkEntrenador _ [] = False
esExperto MkEntrenador ps= tieneDelTipo ps Agua && tieneDelTipo ps Fuego && tieneDelTipo ps Planta 

tieneDelTipo::[Pokemon]->TipoDePokemon->Bool
tieneDelTipo [] t = False
tieneDelTipo (p:ps) t = esDelTipo p t || tieneDelTipo ps t




fiestaPokemon :: [Entrenador] -> [Pokemon]
fiestaPokemon [] = []
fiestaPokemon (e:es)=  (pokemones e) ++ (fiestaPokemon es)

partuzaPokemon:: [Pokemon]->[Pokemon]->[Pokemon]
partuzaPokemon [] [] = []
partuzaPokemon [] [ps2] = [py2]
partuzaPokemon [ps1] [] = [ps1] 
partuzaPokemon (ps:ps1) (py:py2) = ps : py : partuzaPokemon ps1 py2



cantidadDePokemonesIgualesA :: Entrenador->Pokemon->Int
cantidadDePokemonsIgualesA (MkEntrenador _ ps) p = apariciones p ps 

instance Eq Pokemon where
	p1 == p2 = (Tipo p2) && (energia p1) == (energia p2)
	p1 /= p2 = Not (p1 == p2)
	
apariciones :: Eq a => [a]->Int
apariciones e [] = 0
apariciones e (x:xs) = if e == x
			then 1 + (apariciones e xs)
			else apariciones e xs
