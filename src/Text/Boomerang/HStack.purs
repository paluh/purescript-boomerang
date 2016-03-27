module Text.Boomerang.HStack where

import Prelude ((<<<))

data HNil = HNil
data HCons a b = HCons a b

type HTop2 a b t = HCons a (HCons b t)
type HTop3 a b c t = HCons a (HTop2 b c t)
type HTop4 a b c d t = HCons a (HTop3 b c d t)
type HTop5 a b c d e t = HCons a (HTop4 b c d e t)

hNil :: HNil
hNil = HNil

hSingleton :: forall h. h -> HCons h HNil
hSingleton h = HCons h hNil

hCons :: forall h t. h -> t -> HCons h t
hCons = HCons

hHead :: forall h t. HCons h t -> h
hHead (HCons h t) = h

hPop :: forall h t r. (h -> t -> r) -> (HCons h t) -> r
hPop f (HCons h t) = f h t

-- this function nicely compose so it :
-- hArg (hArg (Just
hArg :: forall h h' t r. (h' -> t -> r) -> (h -> h') -> (HCons h t) -> r
hArg ht2r h2h = hPop (ht2r <<< h2h)

hMap :: forall h h' t. (h -> h') -> (HCons h t) -> (HCons h' t)
hMap = hArg HCons
