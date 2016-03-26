module Boomerang.HList where

data HNil = HNil
data HCons a b = HCons a b
nil :: HNil
nil = HNil

type HTop2 a b t = HCons a (HCons b t)
type HTop3 a b c t = HCons a (HTop2 b c t)
type HTop4 a b c d t = HCons a (HTop3 b c d t)
type HTop5 a b c d e t = HCons a (HTop4 b c d e t)

singleton :: forall h. h -> HCons h HNil
singleton h = HCons h nil

cons :: forall h t. h -> t -> HCons h t
cons = HCons

head :: forall h t. HCons h t -> h
head (HCons h t) = h
