import Stack

balanceado :: String -> Bool
balanceado s = chequearParentesis s emptyS

chequearParentesis :: 
	String -> Stack Char -> Bool
chequearParentesis [] s = isEmptyS s
chequearParentesis (x:xs) s =
	not (x == ')' && isEmptyS s) && 
			chequearParentesis xs (chequearChar x s)

chequearChar :: Char -> Stack Char -> Stack Char
chequearChar '(' s = push '(' s
chequearChar ')' s = pop s
chequearChar  x  s = s

	--if x == '('
	--   then chequearParentesis xs (push x s)
	--   else if x == ')'
 --  	          then if isEmptyS s
	--   	            then False
	--   	       	    else chequearParentesis xs (pop s)
	--   	       else chequearParentesis xs s 

	--if x == ')' && isEmptyS s
	--   then False
	--   else chequearParentesis xs (chequearChar x s)
