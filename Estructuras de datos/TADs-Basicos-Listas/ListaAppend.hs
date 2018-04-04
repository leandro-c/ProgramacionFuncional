module ListaAppend(Lista,
	emptyL, 
	isEmptyL,
	headL,
	tailL,
	consL,
	appendL,
	lengthL) where

data ListaAppend a = 
	EmptyL | Unit a | Append (ListaAppend a)
	                         (ListaAppend a)

data Lista a = MkLA (ListaAppend a)

-- Inv.Rep.
---- Sea una lista l = MkLA xs
------- Si xs es Append ys zs
----------- entonces ys y zs no son EmptyL

emptyL :: Lista a
emptyL = MkLA EmptyL

isEmptyL :: Lista a -> Bool
isEmptyL (MkLA xs) = isEmptyRep xs

headL :: Lista a -> a
headL (MkLA xs) = headRep xs

consL :: a -> Lista a -> Lista a
consL x (MkLA xs) = MkLA (consRep x xs)

tailL :: Lista a -> Lista a
tailL (MkLA xs) = MkLA (tailRep xs)

appendL :: Lista a -> Lista a -> Lista a
appendL (MkLA xs) (MkLA ys) =
	MkLA (appendRep xs ys)

lengthL :: Lista a -> Int
lengthL (MkLA xs) = lengthRep xs

isEmptyRep EmptyL = True
isEmptyRep _      = False

headRep (Unit x) = x
headRep (Append xs _) = 
	headRep xs

-- consRep x EmptyL = Unit x
consRep x xs = construirBien (Append (Unit x) xs)

tailRep (Unit x) = EmptyL
tailRep (Append xs ys) = 
	construirBien (Append (tailRep xs) ys)


appendRep xs ys = construirBien (Append xs ys)

lengthRep EmptyL = 0
lengthRep (Unit x) = 1
lengthRep (Append xs ys) = 
	lengthRep xs + lengthRep ys

construirBien :: ListaAppend a -> ListaAppend a
construirBien (Append EmptyL ys) = ys
construirBien (Append xs EmptyL) = xs
construirBien xs                 = xs