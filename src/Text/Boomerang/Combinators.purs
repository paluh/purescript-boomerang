module Text.Boomerang.Combinators where

import Control.Lazy (defer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (flip, id, pure, (<$>), (<>), (<<<))
import Text.Parsing.Parser (Parser)
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Boomerang.HStack (hArg, hCons, hMap, HCons(..), HTop2)

pureBmgSer :: forall a b tok. (a -> Maybe b) -> Serializer tok a b
pureBmgSer s = Serializer ((Tuple id <$> _) <$> s)

pureBmgPrs :: forall a b tok. (a -> b) -> Parser tok (a -> b)
pureBmgPrs p = pure p

pureBmg :: forall a b tok. (a -> b) -> (b -> Maybe a) -> Boomerang tok a b
pureBmg p s =
  Boomerang {
      prs : pureBmgPrs p
    , ser : pureBmgSer s
  }

maph :: forall h h' t tok. (h -> h') -> (h' -> Maybe h) ->
                           Boomerang tok (HCons h t) (HCons h' t)
maph p s =
  pureBmg prs ser
 where
  prs :: HCons h t -> HCons h' t
  prs = hArg hCons p

  ser :: HCons h' t -> Maybe (HCons h t)
  ser =
    hArg hCons' s
   where
    hCons' :: forall a s. Maybe a -> s -> Maybe (HCons a s)
    hCons' mh t  = flip hCons t <$> mh

nil :: forall t a tok. Boomerang tok t (HCons (List a) t)
nil =
  pureBmg (HCons Nil) ser
 where
  ser (HCons Nil t) = Just t
  ser _ = Nothing

cons :: forall tok a t. Boomerang tok (HTop2 a (List a) t) (HCons (List a) t)
cons =
  pureBmg prs ser
 where
  prs = hArg hMap (\lh lt -> lh : lt)
  ser (HCons (Cons lh lt) t) = Just (HCons lh (HCons lt t))
  ser _ = Nothing

list :: forall t a tok. (forall s. Boomerang tok s (HCons a s)) -> Boomerang tok t (HCons (List a) t)
list b = (cons <<< b <<< defer (\_ -> list b)) <> nil

listSep :: forall t a tok. (forall s. Boomerang tok s (HCons a s)) -> (forall r. Boomerang tok r r) -> Boomerang tok t (HCons (List a) t)
listSep b s =
  (cons <<< b <<< defer (\_ -> sepList)) <> nil
 where
  sepList = (s <<< cons <<< b <<< defer (\_ -> sepList)) <> nil

opt :: forall tok t. Boomerang tok t t -> Boomerang tok t t
opt b = b <> id
