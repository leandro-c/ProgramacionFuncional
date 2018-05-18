1. Reducción
1. Reducir las siguientes expresiones hasta una forma normal:
a) id id
b) id id x
c) (*2) . (+2) $ 0
d) flip (-) 2 3
e) all id $ map (const True) [1..5]
f ) map (map (+1)) [[1..5], [6..10]]
g) map ((*2) . (+1)) [1..5]
h) maybe 0 (const 1) $ Just 1
2. Reducir hasta que las siguientes expresiones den True
a) map (+1) [1,2,3] == [2] ++ [3] ++ [4]
b) twice id 5 == (id . id . id) 5
c) maybe 0 (const 2) (Just Nothing) == head (map (+1) [1,2,3])
d) factorial 3 == product [1,2,3]
e) (iterate (1:) []) !! 3 == replicate 3 1
f ) takeWhile (<3) [1,2,3] == map (+1) [0,1]
g) (curry . uncurry) (+) 1 2 == sum (filter (>=1)) [0,0,1,2]
2. Demostraciones simples
Demostrar las siguientes equivalencias entre funciones (pueden ser falsas):
1. last [x] = head [x]

Página 1 de 3

Programación Funcional - UNQ

(\xs -> null x || not (null xs)) = const True
--or [x] == x || not x
--swap . swap = id
--5. twice id = id . id
--6. applyN 2 = twice
7. twice twice = applyN 4
8. (\x -> maybe x id Nothing) = head . (:[])
9. curry (uncurry f) = f
10. uncurry (curry f’) = f’
11. maybe Nothing (Just . const 1) = const (Just 1)
12. apply = id


--Practica 3 
--a
id id --x def id
--b
(id id) x -- ->id x def de id --> x 
-- x def --> def id
--c
(*2) . (+2) $ 0
-- ->((*2).(+2)) 0 -> (*2)((+2)0) -> (*2)(0+2)->(*2) 2 7 4
--d
flip (-) 2 3
--equivalente = flip (\x y -> x -y)
--def de flip 
-- -> (-) 3 2
-- ->3 - 2 -> 1