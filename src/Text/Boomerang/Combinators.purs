module Text.Boomerang.Combinators where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (compose, id, return, (<$>), (<<<), (<>))
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Boomerang.HStack (hArg, hMap, HCons(..), HTop2)

pure :: forall a b tok. (a -> Maybe b) -> (b -> Maybe a) -> Boomerang tok a b
pure p s =
  Boomerang {
      prs : return p
    , ser : Serializer ((Tuple id <$> _) <$> s)
  }

nil :: forall t a tok. Boomerang tok t (HCons (List a) t)
nil =
  pure (Just <<< HCons Nil) ser
 where
  ser (HCons Nil t) = Just t
  ser _ = Nothing

cons :: forall t a tok. Boomerang tok (HTop2 a (List a) t) (HCons (List a) t)
cons =
  pure prs ser
 where
  prs = hArg ((Just <$>) <$> hMap) (\lh lt -> lh : lt)
  ser (HCons (Cons lh lt) t) = Just (HCons lh (HCons lt t))
  ser _ = Nothing

list :: forall t a tok. (forall s. Boomerang tok s (HCons a s)) -> Boomerang tok t (HCons (List a) t)
list b =
   (cons `compose` b `compose` list b) <> nil