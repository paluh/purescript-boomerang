module Text.Boomerang.Prim where

import Control.Alt ((<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad.Eff.Console
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Prelude (bind, compose, class Category, class Semigroupoid,
                class Semigroup, id, return, unit, (<<<))

newtype Serializer tok a b = Serializer (a -> Maybe (Tuple (tok -> tok) b))

instance lazySerializer :: Lazy (Serializer tok a b) where
  defer f = Serializer (\a -> runSerializer (f unit) a)

runSerializer :: forall a b tok. Serializer tok a b -> (a -> Maybe (Tuple (tok -> tok) b))
runSerializer (Serializer f) = f

instance semigroupoidSerializer :: Semigroupoid (Serializer tok) where
  compose (Serializer ser1) (Serializer ser2) = Serializer (\c -> do
    (Tuple t2 b) <- ser2 c
    (Tuple t1 a) <- ser1 b
    return (Tuple (t2 <<< t1) a))

instance categorySerializer :: Category (Serializer tok) where
  id = Serializer (Just <<< Tuple id)

composePrs :: forall tok a b c. Parser tok (b -> c) ->
                                Parser tok (a -> b) ->
                                Parser tok (a -> c)
composePrs prs1 prs2 = do
  b2c <- prs1
  a2b <- prs2
  return (b2c <<< a2b)

type BoomerangRecord tok a b =
  { prs :: Parser tok (a -> b)
  , ser :: Serializer tok b a
  }

data Boomerang tok a b =
  Boomerang (BoomerangRecord tok a b)

instance semigroupoidBoomerang :: Semigroupoid (Boomerang tok) where
  compose (Boomerang b1) (Boomerang b2) =
    Boomerang {
        prs : composePrs b1.prs b2.prs
      , ser : compose b2.ser b1.ser
    }

instance categoryBoomerang :: Category (Boomerang tok) where
  id = Boomerang {
      prs : return id
    , ser : id
  }

instance semigroupBoomerang :: Semigroup (Boomerang tok a b) where
  append (Boomerang b1) (Boomerang b2) =
    Boomerang {
        prs : (try b1.prs) <|> b2.prs
      , ser : Serializer (\b -> (runSerializer b1.ser b) <|> (runSerializer b2.ser b))
    }

instance lazyBoomerang :: Lazy (Boomerang tok a b) where
    defer f =
      Boomerang {
          prs : defer' (_.prs)
        , ser : defer' (_.ser)
      }
     where
      defer' :: forall v. Lazy v => (BoomerangRecord tok a b -> v) -> v
      defer' a = defer (\_ -> (case f unit of (Boomerang b) -> a b))

