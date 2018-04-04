-- Ya existe en Haskell, su definición es:
-- data Maybe = Nothing | Just a

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

-- Ejemplos de recorido recursivo
-- que devuelve Maybe
minT :: Ord a => Tree a -> Maybe a
minT EmptyT = Nothing
minT (NodeT x t1 t2) =
	minM (Just x) (minM (minT t1) (minT t2))

minM :: Ord a => Maybe a -> Maybe a -> Maybe a
minM Nothing m2 = m2
minM m1 Nothing = m1
minM (Just x) (Just y) = Just (min x y)

-- Ya existe en Haskell
-- para usarla hay hacer "import Maybe"
-- fromJust :: Maybe a -> a
-- fromJust (Just x) = x