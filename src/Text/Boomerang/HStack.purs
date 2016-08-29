module Text.Boomerang.HStack where

import Prelude ((<<<))

data HNil = HNil
data HCons a b = HCons a b

infixr 8 type HCons as :-
infixr 8 HCons as :-

class HList r
instance hlistNil :: HList HNil
instance hlistConst :: (HList t) => HList (a :- t)

hTop2 :: forall a b t. a -> b -> t -> a :- b :- t
hTop2 a b t = hCons a (hCons b t)

hTop3 :: forall a b c t. a -> b -> c -> t -> a :- b :- c :- t
hTop3 a b c t = hCons a (hTop2 b c t)

hNil :: HNil
hNil = HNil

hSingleton :: forall h. h -> h :- HNil
hSingleton h = h :- hNil

hCons :: forall h t. h -> t -> h :- t
hCons = HCons

hHead :: forall h t. h :- t -> h
hHead (h :- t) = h

hPop :: forall h t r. (h -> t -> r) -> (h :- t) -> r
hPop f (h :- t) = f h t

-- this function nicely compose so it :
-- hArg (hArg (Just
hArg :: forall h h' t r. (h' -> t -> r) -> (h -> h') -> (h :- t) -> r
hArg ht2r h2h = hPop (ht2r <<< h2h)

hMap :: forall h h' t. (h -> h') -> (h :- t) -> (h' :- t)
hMap = hArg (:-)
