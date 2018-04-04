module Set(Set, emptySet, add, belongs, isEmptySet, setToList, sizeSet) where

data Set a = MkSet [a] Int  deriving (Show,Eq)
--   Tipo        Constructor   Tipo de representacion
--   Abstracto

{- Inv. Rep. 
+ El numero debe ser igual al largo de la Set
-}

-- MkSet :: [a] -> List a

emptySet :: Set a  -- O(1)
emptySet = MkSet [] 0

add  :: Eq a => a -> Set a -> Set a -- O(1)
add x (MkSet xs n) = if elem x xs 
                        then MkSet xs n
						else MkSet (x:xs) (n+1)

belongs :: Eq a => a -> Set a -> Bool -- O(m)
belongs x (MkSet xs n) = elem x xs
			
isEmptySet :: Set a -> Bool -- O(1)
isEmptySet (MkSet xs n) = n == 0

sizeSet :: Set a -> Int
sizeSet (MkSet xs n) = n

setToList :: Set a -> [a] -- O(1)
setToList (MkSet xs _) = xs