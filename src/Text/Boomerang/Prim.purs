module Text.Boomerang.Prim where

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad (class Monad)
import Data.Tuple (Tuple(..))
import Prelude (bind, compose, class Category, class Semigroupoid, class Semigroup, id, pure, unit, (<<<))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)

-- m is a "monad" accumulating serialization results:
-- * if you want to generate all posible serializations you can use [],
-- * if your serialier never failes and you want just one result use Identity
newtype Serializer m tok a b = Serializer (a -> m (Tuple (tok -> tok) b))

instance lazySerializer :: Lazy (Serializer m tok a b) where
  defer f = Serializer (\a -> runSerializer (f unit) a)

runSerializer :: forall a b m tok. Serializer m tok a b -> (a -> m (Tuple (tok -> tok) b))
runSerializer (Serializer f) = f

instance semigroupoidSerializer :: (Monad m) => Semigroupoid (Serializer m tok) where
  compose (Serializer ser1) (Serializer ser2) = Serializer (\c -> do
    (Tuple t2 b) <- ser2 c
    (Tuple t1 a) <- ser1 b
    pure (Tuple (t2 <<< t1) a))

instance categorySerializer :: (Monad m) => Category (Serializer m tok) where
  id = Serializer (pure <<< Tuple id)

composePrs :: forall tok a b c. Parser tok (b -> c) ->
                                Parser tok (a -> b) ->
                                Parser tok (a -> c)
composePrs prs1 prs2 = do
  b2c <- prs1
  a2b <- prs2
  pure (b2c <<< a2b)

type BoomerangRecord m tok a b =
  { prs :: Parser tok (a -> b)
  , ser :: Serializer m tok b a
  }

data Boomerang m tok a b =
  Boomerang (BoomerangRecord m tok a b)

instance semigroupoidBoomerang :: (Monad m) => Semigroupoid (Boomerang m tok) where
  compose (Boomerang b1) (Boomerang b2) =
    Boomerang {
        prs : composePrs b1.prs b2.prs
      , ser : compose b2.ser b1.ser
    }

instance categoryBoomerang :: (Monad m) => Category (Boomerang m tok) where
  id = Boomerang {
      prs : pure id
    , ser : id
  }

instance semigroupBoomerang :: (Alt m) => Semigroup (Boomerang m tok a b) where
  append (Boomerang b1) (Boomerang b2) =
    Boomerang {
        prs : (try b1.prs) <|> b2.prs
      , ser : Serializer (\b -> (runSerializer b1.ser b) <|> (runSerializer b2.ser b))
    }

instance lazyBoomerang :: Lazy (Boomerang m tok a b) where
  defer f =
    Boomerang {
        prs : defer' (_.prs)
      , ser : defer' (_.ser)
    }
   where
    defer' :: forall v. Lazy v => (BoomerangRecord m tok a b -> v) -> v
    defer' a = defer (\_ -> (case f unit of (Boomerang b) -> a b))
