module Quizas(...)where

data Quizas q = Q(Maybe a)


nothing :: Quizas a
nothing Q(Nothing a)

just :: a -> Quizas a
just n = Q(Just a)

fromJust :: Quizas a -> a
fromJust Q(m) = fromJust a


isNothing :: Quizas a -> Bool