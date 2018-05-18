recr::b -> ( a -> b -> b ) -> [a] -> b
recr z f [] = z
recr z f ( x : xs' ) = f x xs' recr z f xs'

foldr ::(a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
--foldr1 :: (a -> a -> a) -> [a] -> a
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr 
