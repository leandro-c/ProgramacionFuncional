data Tree a = Nil | Node a (Tree a) (Tree a)

pepe = Node 2 
		(Node 4 
			Nil 
			Nil)
			
		(Node 5
			Nil
			(Node 9
				Nil	
				(Node 5
					(Node 4 
						Nil 
						(Node 9
							Nil
							Nil)
					)
					(Node 4 
						Nil 
						(Node 9
							Nil
							Nil)
					)
				)	
			)
		)
		
pepeL = Node [1,2,3]
			(Node [7,7,7]
					Nil
					Nil
			)
			(Node [8,8,8]
					Nil
					Nil
			)
			
pepeI = Node 1
			(Node 2
					Nil
					Nil
			)
			(Node 3
					Nil
					Nil
			)
sumT :: Tree Integer -> Integer
sumT Nil = 0
sumT (Node n l r) = n + sumT l + sumT r

sizeT :: Tree a -> Integer
sizeT Nil = 0
sizeT (Node n l r) = 1 + sizeT l + sizeT r

mapDoubleT :: Tree Integer -> Tree Integer
mapDoubleT Nil = Nil
mapDoubleT (Node n l r) = Node (2*n)  (mapDoubleT l)  (mapDoubleT r)

elemT :: Eq a => a -> Tree a -> Bool
elemT a Nil = False
elemT a (Node b l r) = a == b || elemT a l || elemT a r

occurrsT :: Eq a => a -> Tree a -> Int
occurrsT a Nil = 0
occurrsT a (Node b l r) = if a == b
							then 1 + (occurrsT a l) + (occurrsT a r)
							else (occurrsT a l) + (occurrsT a r)
							
countLeaves :: Tree a -> Int
countLeaves Nil = 0
countLeaves (Node n Nil Nil) = 1
countLeaves (Node n l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Nil = []
leaves (Node n Nil Nil) = [n]
leaves (Node n l r) = leaves l ++ (leaves r)

heightT :: Tree a -> Int
heightT Nil = 0
heightT (Node n l r) = if heightT l > heightT r
						then heightT l + 1
						else heightT r + 1
						
countNoLeaves :: Tree a -> Int
countNoLeaves Nil = 0
countNoLeaves (Node n Nil Nil) = 0
countNoLeaves (Node n l r) = 1 + countLeaves l + countNoLeaves r

mirrorT :: Tree a -> Tree a
mirrorT Nil = Nil
mirrorT (Node n l r) = Node n (mirrorT r) (mirrorT l)

listInOrder :: Tree a -> [a]
listInOrder Nil = []
listInOrder (Node n l r) = (listInOrder l) ++ [n] ++ (listInOrder r) 

listPreOrder :: Tree a -> [a]
listPreOrder Nil = []
listPreOrder (Node n l r) = [n] ++ (listPreOrder l) ++ (listPreOrder r) 

listPosOrder :: Tree a -> [a]
listPosOrder Nil = []
listPosOrder (Node n l r) = (listPosOrder l) ++ (listPosOrder r) ++ [n] 

concatT :: Tree [a] -> [a]
concatT Nil = []
concatT (Node n Nil Nil) = n
concatT (Node n l r) = (concatT l) ++ n ++ (concatT r) 

levelN :: Int -> Tree a -> [a]
leveln n Nil = []
levelN n (Node b l r) = if n == 0
							then [b]
							else levelN (n-1) l ++ levelN (n-1) r

							
							
listPerLevel :: Tree a -> [[a]]
listPerLevel Nil = []
listPerLevel (Node a l r) = [[a]] ++ (listPerLevel l) ++ (listPerLevel r)

allT :: (a -> Bool) -> Tree a -> Bool
allT f Nil = True
allT f (Node r i d) = f r && (allT f i) && (allT f d)

andT Nil = True
andT (Node r i d) = r && andT i && andT d

anyT p Nil = False
anyT p (Node r i d) = p x || anyT p i || anyT p d

lengthT EmptyT = 0
lengthT (Node r ii¿ d) = 1 + lengthT i + lengthT d

mapT f Nil = Nil
mapT f (Node r l d) = Node (f r) (mapT f l) (mapT f r)

const x y = x