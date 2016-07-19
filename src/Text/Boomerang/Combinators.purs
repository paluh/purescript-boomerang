module Text.Boomerang.Combinators where

import Control.Alt (class Alt)
import Control.Applicative (class Applicative)
import Control.Lazy (defer)
import Control.Monad (class Monad)
import Control.Plus (class Plus, empty)
import Data.Functor (class Functor)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Prelude (flip, id, pure, (<$>), (<>), (<<<))
import Text.Boomerang.HStack (hArg, hCons, hMap, HCons(..), HTop2)
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Parsing.Parser (Parser)

pureBmgSer :: forall a b m tok. (Functor m) => (a -> m b) -> Serializer m tok a b
pureBmgSer s = Serializer ((Tuple id <$> _) <$> s)

pureBmgPrs :: forall a b tok. (a -> b) -> Parser tok (a -> b)
pureBmgPrs p = pure p

pureBmg :: forall a b m tok. (Functor m) => (a -> b) -> (b -> m a) -> Boomerang m tok a b
pureBmg p s =
  Boomerang {
      prs : pureBmgPrs p
    , ser : pureBmgSer s
  }

maph :: forall h h' m t tok. (Functor m) =>
                             (h -> h') -> (h' -> m h) ->
                             Boomerang m tok (HCons h t) (HCons h' t)
maph p s =
  pureBmg prs ser
 where
  prs :: HCons h t -> HCons h' t
  prs = hArg hCons p

  ser :: HCons h' t -> m (HCons h t)
  ser =
    hArg hCons' s
   where
    hCons' :: forall a s. m a -> s -> m (HCons a s)
    hCons' mh t  = flip hCons t <$> mh

nil :: forall a m t tok. (Applicative m, Plus m) => Boomerang m tok t (HCons (List a) t)
nil =
  pureBmg (HCons Nil) ser
 where
  ser (HCons Nil t) = pure t
  ser _ = empty

cons :: forall a m t tok. (Applicative m, Plus m) =>
                          Boomerang m tok (HTop2 a (List a) t) (HCons (List a) t)
cons =
  pureBmg prs ser
 where
  prs = hArg hMap (\lh lt -> lh : lt)
  ser (HCons (Cons lh lt) t) = pure (HCons lh (HCons lt t))
  ser _ = empty

list :: forall a m t tok. (Plus m, Monad m) => (forall s. Boomerang m tok s (HCons a s)) -> Boomerang m tok t (HCons (List a) t)
list b = (cons <<< b <<< defer (\_ -> list b)) <> nil

listSep :: forall a m t tok. (Plus m, Monad m) => (forall s. Boomerang m tok s (HCons a s)) -> (forall r. Boomerang m tok r r) -> Boomerang m tok t (HCons (List a) t)
listSep b s =
  (cons <<< b <<< defer (\_ -> sepList)) <> nil
 where
  sepList = (s <<< cons <<< b <<< defer (\_ -> sepList)) <> nil

opt :: forall m tok t. (Alt m, Monad m) => Boomerang m tok t t -> Boomerang m tok t t
opt b = b <> id
