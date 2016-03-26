module Text.Boomerang.Prim where

import Control.Monad.Eff.Console
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Prelude (bind, compose, class Category, class Semigroupoid, id, return, (<<<), (>>=))

newtype Serializer tok a b = Serializer (a -> Maybe (Tuple (tok -> tok) b))

instance semigroupoidSerializer :: Semigroupoid (Serializer tok) where
  compose (Serializer ser1) (Serializer ser2) = Serializer (\c -> do
    (Tuple t2 b) <- ser2 c
    (Tuple t1 a) <- ser1 b
    return (Tuple (t2 <<< t1) a))

instance categorySerializer :: Category (Serializer tok) where
  id = Serializer (Just <<< (Tuple id) <<< id)

composePrs :: forall tok a b c. Parser tok (b -> Maybe c) ->
                                Parser tok (a -> Maybe b) ->
                                Parser tok (a -> Maybe c)
composePrs prs1 prs2 = do
  a2mb <- prs2
  b2mc <- prs1
  return (\a -> (a2mb a >>= b2mc))

data Boomerang tok a b =
  Boomerang {
      prs :: Parser tok (a -> Maybe b)
    , ser :: Serializer tok b a
  }

instance semigroupoidBoomerang :: Semigroupoid (Boomerang tok) where
  compose (Boomerang b1) (Boomerang b2) =
    Boomerang {
        prs : composePrs (b1.prs) (b2.prs)
      , ser : compose (b2.ser) (b1.ser)
    }

instance boomerangCategory :: Category (Boomerang tok) where
  id = Boomerang {
      prs : return (Just <<< id)
    , ser : id
  }
