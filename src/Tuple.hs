module Tuple where

onFst :: (a -> a -> c) -> (a,b) -> (a,d) -> c
onFst f a b = f (fst a) (fst b)

onSnd :: (a -> a -> c) -> (b,a) -> (d,a) -> c
onSnd f a b = f (snd a) (snd b)