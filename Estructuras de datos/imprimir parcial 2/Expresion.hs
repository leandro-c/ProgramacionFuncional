data Expresion = Suma Int Expresion | Mult Int Expresion | Num Int
data Lista a = Vacio | Cons a(Lista a)
data ListaNV a = Unit a |Cons a (ListaNV a)

cantSumas::Expresion -> Int
cantSumas (Num n) = 0
cantSumas (Mult n exp) = cantSumas exp
cantSumas (Suma n exp) = 1 + cantSumas exp 

eval::Expresion -> Int
eval Num n = n
eval Suma n exp = n + eval exp
eval Mul n exp = n * eval exp

numMayoresA::Expresion -> Int -> [Int]
numMayoresA (Num n) y = agregarMayor n y
numMayoresA (Sum n exp) y = agregarMayor n y : numMayoresA exp y--aca no van los :?? 
numMayoresA (Mul n exp) y = agregarMayor n y : numMayoresA exp y--aca no van los :??

agregarMayor::Int -> Int -> [Int]
agregarMayor n y = if(n > y)	
			then [n]
			else []
aLista::[a] -> Lista a
aLista [] = Vacio
aLista (a:as) = Cons x (aLista as)
 
desdeLista:: Lista a -> [a]
deLista Vacio = []
deLista(Cons x xs) = x:desdeLista xs

minimoLNV::ListaNV Int-> Int
minimoLNV Unit n = n
minimoLNV (Cons x xs) = min x (minimoLNV xs)

minimo::[Int]->Int
--Parcial en []
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

aListNV:: [a] -> ListNV a
--Parcial en []
