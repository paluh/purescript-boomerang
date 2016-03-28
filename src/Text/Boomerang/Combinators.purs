module Text.Boomerang.Combinators where

import Control.Lazy (defer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (compose, flip, id, return, (<$>), (<>), unit)
import Text.Parsing.Parser (Parser)
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Boomerang.HStack (hArg, hCons, hMap, HCons(..), HTop2)

pureSer :: forall a b tok. (a -> Maybe b) -> Serializer tok a b
pureSer s = Serializer ((Tuple id <$> _) <$> s)

purePrs :: forall a b tok. (a -> b) -> Parser tok (a -> b)
purePrs p = return p

pure :: forall a b tok. (a -> b) -> (b -> Maybe a) -> Boomerang tok a b
pure p s =
  Boomerang {
      prs : purePrs p
    , ser : pureSer s
  }

maph :: forall h h' t tok. (h -> h') -> (h' -> Maybe h) ->
                           Boomerang tok (HCons h t) (HCons h' t)
maph p s =
  pure prs ser
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
  pure (HCons Nil) ser
 where
  ser (HCons Nil t) = Just t
  ser _ = Nothing

cons :: forall t a tok. Boomerang tok (HTop2 a (List a) t) (HCons (List a) t)
cons =
  pure prs ser
 where
  prs = hArg hMap (\lh lt -> lh : lt)
  ser (HCons (Cons lh lt) t) = Just (HCons lh (HCons lt t))
  ser _ = Nothing

list :: forall t a tok. (forall s. Boomerang tok s (HCons a s)) -> Boomerang tok t (HCons (List a) t)
list b = (cons `compose` b `compose` defer (\_ -> list b)) <> nil
