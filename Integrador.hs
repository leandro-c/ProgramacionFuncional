
data ThreeT a = Leaf a | Branch (ThreeT a) (ThreeT a) (ThreeT a)

sizeTT :: ThreeT a -> Int
sizeTT (Leaf a) = 1
sizeTT (Branch t1 t2 t3) = sizeTT t1 + sizeTT t2 + sizeTT t3

sumTT :: ThreeT Int -> ThreeT Int
sumTT (Leaf a) = a
sumTT (Branch t1 t2 t3) = sumTT t1 + sumTT t2 + sumTT t3

leavesTT :: ThreeT a -> [a]
leavesTT (Leaf a) = [a]
leavesTT (Branch t1 t2 t3) =(leavesTT t1)++(leavesTT t2)++(leavesTT t3)

--Devuelve los elementos que están en las hojas
mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f (Leaf a) = Leaf (f a)
mapTT f (Branch t1 t2 t3) = (Branch (mapTT f t1)(mapTT f t2)(mapTT f t3))

maxTT :: Ord a => ThreeT a -> a
maxTT (Leaf a) = a
maxTT (Branch t1 t2 t3) = max (max (maxTT t1) (maxTT t2) )(maxTT t3)

findTT :: (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT f Leaf (a,b) = if f a then Just b else Nothing --aca se puede usar isf
findTT f (Branch t1 t2 t3) = returnValue(returnValue((findTT f t1) (findTT f t2)) (findTT f t3))

isF :: (a->Bool)->(a,b)->Maybe b
isF True (a,b) = Just b
isF False _ = Nothing

returnValue:: Maybe b -> Maybe b -> Maybe b
returnValue Nothing Nothing = Nothing
returnValue (Just a) Nothing = Just a
returnValue Nothing (Just a) = Just a

levelNTT :: Int -> ThreeT a -> [a]
levelNTT 0 (Leaf a) = [a]
levelNTT n (Leaf a) = []
levelNTT 0 (Branch t1 t2 t3) = []
levelNTT n (Branch t1 t2 t3) = (levelNTT (n-1) t1)++(levelNTT (n-1) t2)++(levelNTT (n-1) t3)

listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT (Leaf a) = [[a]]
listPerLevelTT (Branch t1 t2 t3) = concatL(concatL((listPerLevelTT t1) (listPerLevelTT t2)) (listPerLevelTT t3))

concatL::[a]->[b]->[b]
concatL [] ys = ys
concatL xs [] = xs
concatL (x:xs)(y:ys) = x++y :concatL xs ys
--Devuelve una lista de listas donde en cada lista están los elementos de cada nivel.
{-
2. Dar una definición de fold (llamada foldTT) en base a la estructura del tipo ThreeT.
3. Definir las funciones del primer punto usando la definición dada de fold para ThreeT.
4. Demostrar las siguientes equivalencias usando las funciones definidas en el punto 1.
a) sizeTT = sumTT . mapTT (const 1)
b) sum . leavesTT = sumTT
c) sizeTT . mapTT f . mapTT g = sizeTT . mapTT (f . g)
d) maximum . leavesTT = maxTT
-}
--Estoy viendo los ejercicos que resolviste en clase el LTree
foldTT::(a->b)->(a->b->b->b->b)->ThreeT a -> b
foldTT f g (Leaf a) = f a
foldTT f g (Branch t1 t2 t3) = f (foldTT f g t1) (foldTT f g  t2) (foldTT f g  t3)

-------------------------------------------con fold----------------------
sizeTT :: ThreeT a -> Int
sizeTT = foldTT (\x->1)(\x r1 r2 r3 -> 1 + r1 + r2 + r3)

sumTT :: ThreeT Int -> ThreeT Int
sumTT = foldTT (\x->x)(\x r1 r2 r3 -> x + r1 + r2 + r3)

leavesTT :: ThreeT a -> [a]
leavesTT = foldTT (\a -> [a]) (\t1 t2 t3 ->t1++t2++t3)
{-levelNF n t = foldLT g h t n
   where casoL xs 0 = xs
         casoL xs n = []
         casoB x r1 r2 0 = [x]
         casoB x r1 r2 n = r1 (n-1) ++ r2 (n-1)
-}
mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f = foldTT (Leaf f a)(\t1 t2 t3 ->Branch( (t1 f) (t2 f) (t3 f)) )

maxTT :: Ord a => ThreeT a -> a
maxTT = foldTT (\a->a) (\t1 t2 t3 -> max(max t2 t3)t1)

findTT :: (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT = foldTT (\(x,y)->if f x then Just y else Nothing) (\t1 t2 t3 -> returnValue(returnValue t2 t3)t1)

levelNTT :: Int -> ThreeT a -> [a]
levelNTT = foldTT fl fb t n
where fl x 0 = [x]
      fl x n = []
      fb t1 t2 t3 0 = []
      fb t1 t2 t3 n =t1(n-1)++t2(n-1)++t3(n-1)

listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT = foldTT (\x->[[x]])(\t1 t2 t3->concatL(concatL t1 t2)t3)

{-
sizeTT.mapTT f.mapTT g = sizeTT.mapTT(f.g)

maximum . leavesTT = maxTT
por prin de ext
(maximum.leavesTT)t = maxTT t
por induccion en t
caso base t = Leaf x
maxTT (Leaf x)
=       def maxTT
x

otro lado
maximum(leavesTT(leaf x))
=             def leaves TT
maximum [x]
=           def maximum.1

x
Paso Inductivo
hi) maximum(leavesTT t1) = maxTT t1
    maximum(leavesTT t2) = maxTT t2
    maximum(leavesTT t3) = maxTT t3
ti) maximum(leavesTT(Branch t1 t2 t3))=maxTT(Branch t1 t2 t3)

maximum(leavesTT (Branch t1 t2 t3))
=                   def leaves
maximum(leavesTT t1 ++ t2 ++ t3)
=
maximum(leavesTT t1)'max'maximum(leavesTT t2)'max'maximum(leavesTT t3)
=   por hi
maxTT t1 'max' maxTT t2 'max' maxTT t3


-}
{-
--funciones por arboles
width::Tree a->Int
elemPerPath::Tree a->[[a]]
elemPerPath EmptyT = []
elemPerPath (NodeT x t1 t2) =  map (x:) (elemPerPath t1 ++  elemPerPath t2)


elemsOfLongstPath::Tree a->[a]
elemsOfLongstPath EmptyT = []
elemsOfLongstPath (NodeT x t1 t2) =
  let e1= elemsOfLongstPath t1
      e2= elemsOfLongstPath t2
      in if length e1 > length e2 then x:e1 else x:e2


unfoldr::(b->Maybe (a,b))->(b->[a])
unfoldr f b = case f b of
                        Nothing->[]
                        Just(a,b)->a:unfoldr f b'

repeat' x = unfoldr(\b->Just(b,b)) x
countDown n = unfoldr(\b if b==0 then Just b else Just(b,b-1))n

scanr::(a->b-b)->b->[a]->[b]
scanr f z []  = [z]
scanr f z (x:xs) =
  let (b:bs) = scanr f z xs
  in f x b : xs
  --foldr f z (x:xs):scanr f z xs
fix::(a->a)->a
-}









