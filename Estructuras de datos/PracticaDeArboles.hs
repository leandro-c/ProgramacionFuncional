data Tree a = Empty | NodeT a (Tree a)(Tree a)

data Colores = Rojo | Negro | Azul 
data Persona = MkPersona Nombre Edad

type Nombre = String
type Edad = Int

ej::Tree Int 
ej = (NodeT 10 (NodeT 5(NodeT 1 Empty Empty)(NodeT 2 Empty Empty))(NodeT 1 Empty Empty))

edad::Persona->Edad
edad (MkPersona _ e ) = e

--isEmptyT EmptyT=True
--isEmptyT(NodeT x t1 t2)=False
--rootT(NodeT x t1 t2)=x
--leftT(NodeT x t1 t2)=t1
--rightT(NodeT x t1 t2)=t2

sumarT::Tree Int -> Int
sumarT Empty = 0
sumarT (NodeT x t1 t2) = x + (sumarT t1)+(sumarT t2)

contarT::Tree a -> Int
contarT Empty = 0
contarT (NodeT x t1 t2) = 1 + (contarT t1)+(contarT t2)

--Dado un  ́arbol binario de enteros devuelve la suma entre sus elementos.
--Dado un  ́arbol binario devuelve su cantidad de elementos, es decir, el tama ̃no del  ́arbol (size en ingles).
sizeT::Tree a -> Int
sizeT Empty = 0
sizeT (NodeT x t1 t2) = 1 + (sizeT t1)+ (sizeT t2)

--Dado un  ́arbol de enteros devuelve un  ́arbol con el doble de cada numero.
mapDobleT::Tree Int -> Tree Int
mapDobleT Empty = Empty
mapDobleT (NodeT x t1 t2)= NodeT (x*2) (mapDobleT t1)(mapDobleT t2)

--Dado un  ́arbol de direcciones t devuelve un  ́arbol con la direcci ́on opuesta para cada elemento de t.
--Reutilizar la funcion Dir
--mapOpuestoT::Tree Dir -> Tree Dir
--mapOpuestoT Empty = Empty 
--mapOpuestoT (NodeT x t1 t2) = NodeT (Dir x) (mapOpuestoT t1) (mapOpuestoT t2)

--length ya esta definido 
mapLongitudT::Tree String -> Tree Int
mapLongitudT Empty = Empty
mapLongitudT (NodeT x t1 t2) =  NodeT (length x) (mapLongitudT t1)(mapLongitudT t2) 


--Dados un elemento y un  ́arbol binario devuelve True si existe un elemento igual a ese en el arbol.

perteneceT::Eq a => a -> Tree a -> Bool
perteneceT a Empty =  False
perteneceT a (NodeT x t1 t2) =  (a == x) || (perteneceT a t1) || (perteneceT a t2)

--Dados un elemento e y un  ́arbol binario devuelve la cantidad de elementos del  ́arbol que son
--iguales a e.
aparicionesT::Eq a => a -> Tree a -> Int
aparicionesT  a Empty = 0
aparicionesT a (NodeT x t1 t2) = if (x == a)
									then 1 + (aparicionesT a t1)+(aparicionesT a t2)
									else (aparicionesT a t1)+(aparicionesT a t2)


--Dado un  ́arbol de personas devuelve el promedio entre las edades de todas las personas.
--Definir las subtareas que sean necesarias para resolver esta funci ́on.
--Nota: Utilizar el tipo Persona ya definido.
promedioEdadesT::Tree Persona -> Int
promedioEdadesT Empty = 0 
promedioEdadesT tree =  div(sumarEdadesT tree)(sizeT tree)

sumarEdadesT::Tree Persona -> Int
sumarEdadesT Empty = 0 
sumarEdadesT (NodeT n izq der) = (edad  n) + (sumarEdadesT izq)+(sumarEdadesT der)
--otro caso 
--promedioEdadesT t = div(SumartT(mapEdades t))(size t)
--mapEdades::Tree Persona -> Tree Int
--mapEdades (NodeT x t1 t2) = NodeT (edad x)(mapEdades t1)(mapEdades t2)


--NOTA: la mayoria de la veces que aparece Eq hay que hacer un ==
--map barre la estructura y le hace algo a todos los elementos
--Los filter tiene otra estructura , hacen un if y retornan otra estructura segun esa condicion


--Dados dos  ́arboles construye un  ́arbol t en el que ambos  ́arboles son hijos de t, y en la ra ́ız
--de t se guarda la suma de todos los elementos de los hijos de t. ¿Se utiliza recursion para
--definir esta funcion?
engancharYSumaEnRaiz::Tree Int -> Tree Int -> Tree Int
engancharYSumaEnRaiz Empty Empty = Empty 
engancharYSumaEnRaiz t1 t2  = NodeT (contarHijos t1 t2) t1 t2

contarHijos::Tree Int->Tree Int->Int
contarHijos t1 t2 = contarT t1 + contarT t2 

--Dado un  ́arbol devuelve su cantidad de hojas.
--Nota: una hoja (leaf en ingl ́es) es un nodo que no tiene hijos.
leaves::Tree a -> Int
leaves Empty = 0 
leaves (NodeT x Empty Empty) = 1
leaves (NodeT x t1 t2) =  (leaves t1) + (leaves t2)


--Dado un  ́arbol devuelve su altura.
--Nota: la altura (height en ingles) de un  ́arbol es la cantidad maxima de nodos entre la raız
--y alguna de sus hojas. La altura de un  ́arbol vac ́ıo es cero y la de una hoja es 1.


espejoT::Tree a -> Tree a
espejoT Empty = Empty
espejoT (NodeT x t1 t2) = NodeT x (espejoT t2)(espejoT t1)



--Dados un numero n y un  ́arbol devuelve una lista con los nodos de nivel n.
--Nota: El primer nivel de un  ́arbol (su ra ́ız) es 0.
levelN :: Int -> Tree a -> [a]
levelN n Empty = [] 
levelN 0 (NodeT x Empty Empty) = [x]
levelN n (NodeT x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2) 


--Dado un  ́arbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho  ́arbol.
--quiere que pongas todos los niveles posibles en una lista
--eso significa que lo que te conviene hacer
--es llamar a levelN
--por cada nivel posible en el arbol
--(height t) te da la altura del arbol, es decir, el numero de ultimo nivel
--entonces tenes que ir de 0 a height t
--sin hacer recursion sobre el arbol
--solo sobre el número
--listPerLevel t = otroListPerLevel t (height t)
--otroListPerLevel t n = levelN t n : otroListPerLevel t (n-1)
listPerLevel :: Tree a -> [[a]]
listPerLevel Empty = [[]]
listPerLevel (NodeT x Empty Empty) = [[x]]
listPerLevel t = otroListPerLevel (heightT t) t

otroListPerLevel:: Int -> Tree a -> [[a]]
otroListPerLevel n Empty  = []
otroListPerLevel n t  = levelN n t : (otroListPerLevel (n-1) t)

--Dado un  ́arbol devuelve su ancho (width en ingl ́es), que es la cantidad de nodos del nivel
--con mayor cantidad de nodos.
widthT :: Tree a -> Int
widthT Empty = 0 
widthT (NodeT x Empty Empty) = 1
widthT (NodeT x t1 t2)  = widthT t1 + widthT t2
