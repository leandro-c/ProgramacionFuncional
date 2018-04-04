separarComoponentes::[(a,a)]->([a],[a])
separarComoponentes ((x,y):ps) = let (xs,ys) = separarComoponentes ps  
						   in (x:xs ,y:ys)

--prec.: la lista no es vacia 
splitMin::Ord a =>[a]->(a,[a])
splitMin x:xs  = let(m,sinM) = splitMin  xs
					in (min x m , max x m : sinM )						   
--[(1,2),(2,4),(5,6)]->([1,2,5],[3,4,6])					

--Usando splitMin , Prec .: ninguna sublista es vacia
minimos::Eq a => [[a]] -> [a]
minimos xs:xss = let (x,s) = splitMin xs
				in x : minimos xss


--prec.: las claves de [k] existen Map k v
--usar let para obtener el resultado del lookup
obtenerClaves::Ord k => [k] ->Map k v ->[v]
obtenerClaves