module Text.Boomerang.Prim where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (StateT(..), runStateT)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either(Left, Right))
import Data.Filterable (partitionMap)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(Tuple))
import Text.Parsing.Parser (ParseError, ParseState(ParseState), ParserT(ParserT))
import Text.Parsing.Parser.Pos (Position)

newtype Parsers tok a = Parsers (ParserT tok List a)
derive instance newtypeParsers :: Newtype (Parsers tok a) _
derive newtype instance functorParsers :: Functor (Parsers tok)
derive newtype instance applyParsers :: Apply (Parsers tok)
derive newtype instance applicativeParsers :: Applicative (Parsers tok)
derive newtype instance monadThrowParsers :: MonadThrow ParseError (Parsers tok)
derive newtype instance monadErrorParsers :: MonadError ParseError (Parsers tok)
derive newtype instance monadStateParsers :: MonadState (ParseState tok) (Parsers tok)
derive newtype instance lazyParsers :: Lazy (Parsers tok a)

instance bindParsers :: Bind (Parsers tok) where
  bind ps f =
    (Parsers <<< ParserT <<< ExceptT <<< StateT) $ \(ParseState input pos _) ->
      case runParsers' input pos ps of
        { left: errs, right: Nil } -> map (\(Tuple e s) -> Tuple (Left e) s) errs
        { left: _, right: as } -> do
          (Tuple a (ParseState input' pos' _)) <- as
          runParsers input' pos' (f a)

instance monadParsers :: Monad (Parsers tok)

runParsers :: forall a tok. tok -> Position -> Parsers tok a -> List (Tuple (Either ParseError a) (ParseState tok))
runParsers s pos (Parsers p) =
  runStateT (runExceptT (unwrap p)) initialState
 where
  initialState = ParseState s pos false

runParsers' :: forall a tok
  . tok
  -> Position
  -> Parsers tok a
  -> { left ∷ List (Tuple ParseError (ParseState tok))
     , right ∷ List (Tuple a (ParseState tok)) }
runParsers' input pos prs =
  partitionMap p (runParsers input pos prs)
 where
  p (Tuple (Right a) s) = Right <<< Tuple a $ s
  p (Tuple (Left e) s) = Left <<< Tuple e $ s

instance altParsers :: Alt (Parsers tok) where
  alt p1 p2 =
    (Parsers <<< ParserT <<< ExceptT <<< StateT) \(s@(ParseState i p _)) ->
      runParsers i p p1 <> runParsers i p p2

composePrs :: forall tok a b c. Parsers tok (b -> c) ->
                                Parsers tok (a -> b) ->
                                Parsers tok (a -> c)
composePrs prs1 prs2 = do
  b2c <- prs1
  a2b <- prs2
  pure (b2c <<< a2b)


newtype Serializer tok a b = Serializer (a -> Maybe (Tuple (tok -> tok) b))

instance lazySerializer :: Lazy (Serializer tok a b) where
  defer f = Serializer (\a -> runSerializer (f unit) a)

runSerializer :: forall a b tok. Serializer tok a b -> (a -> Maybe (Tuple (tok -> tok) b))
runSerializer (Serializer f) = f

instance semigroupoidSerializer :: Semigroupoid (Serializer tok) where
  compose (Serializer ser1) (Serializer ser2) = Serializer (\c -> do
    (Tuple t2 b) <- ser2 c
    (Tuple t1 a) <- ser1 b
    pure (Tuple (t2 <<< t1) a))

instance categorySerializer :: Category (Serializer tok) where
  id = Serializer (Just <<< Tuple id)

type BoomerangRecord tok a b =
  { prs :: Parsers tok (a -> b)
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
      prs : pure id
    , ser : id
  }

instance semigroupBoomerang :: Semigroup (Boomerang tok a b) where
  append (Boomerang b1) (Boomerang b2) =
    Boomerang
      { prs: b1.prs <|> b2.prs
      , ser: Serializer (\b -> (runSerializer b1.ser b) <|> (runSerializer b2.ser b))
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

