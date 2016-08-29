module Text.Boomerang.Combinators where

import Control.Lazy (defer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..))
import Prelude
import Text.Boomerang.HStack (hArg, hCons, hMap, type (:-), (:-))
import Text.Boomerang.Prim (runSerializer, Boomerang(..), Serializer(..))
import Text.Parsing.Parser (Parser)

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
                           Boomerang tok (h :- t) (h' :- t)
maph p s =
  pureBmg prs ser
 where
  prs :: h :- t -> h' :- t
  prs = hArg hCons p

  ser :: h' :- t -> Maybe (h :- t)
  ser =
    hArg hCons' s
   where
    hCons' :: forall a s. Maybe a -> s -> Maybe (a :- s)
    hCons' mh t  = flip hCons t <$> mh

duck1 :: forall a h t tok. Boomerang tok t (a :- t) -> Boomerang tok (h :- t) (a :- h :- t)
duck1 (Boomerang b) =
  Boomerang
    { prs: prs
    , ser: ser
    }
 where
  prs =
    (\t2at -> \(h :- t) -> case t2at t of (a :- t') -> a :- h :- t') <$> b.prs
  ser =
    Serializer (\(a :- h :- t) -> second (h :- _) <$> runSerializer b.ser (a :- t))

nil :: forall t a tok. Boomerang tok t (List a :- t)
nil =
  pureBmg ((:-) Nil) ser
 where
  ser (Nil :- t) = Just t
  ser _ = Nothing

cons :: forall tok a t. Boomerang tok (a :- (List a) :- t) (List a :- t)
cons =
  pureBmg prs ser
 where
  prs = hArg hMap (\lh lt -> lh : lt)
  ser ((lh : lt) :- t) = Just (lh :- lt :- t)
  ser _ = Nothing

list :: forall t a tok. (forall s. Boomerang tok s (a :- s)) -> Boomerang tok t (List a :- t)
list b = (cons <<< b <<< defer (\_ -> list b)) <> nil

listSep :: forall t a tok. (forall s. Boomerang tok s (a :- s)) -> (forall r. Boomerang tok r r) -> Boomerang tok t (List a :- t)
listSep b s =
  (cons <<< b <<< defer (\_ -> sepList)) <> nil
 where
  sepList = (s <<< cons <<< b <<< defer (\_ -> sepList)) <> nil

opt :: forall tok t. Boomerang tok t t -> Boomerang tok t t
opt b = b <> id
