module Text.Boomerang.String where

import Prelude
import Data.Array as Array
import Text.Parsing.Parser.String as Parser.String
import Control.Error.Util (hush)
import Control.Plus (empty, class Plus)
import Data.Foldable (class Foldable, elem)
import Data.Int as Int
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Text.Boomerang.Combinators (list, cons, pureBmg)
import Text.Boomerang.HStack (hCons, HCons(..), hHead, hMap, hNil, HNil, hSingleton)
import Text.Boomerang.Prim (Boomerang(..), runSerializer, Serializer(..))
import Text.Parsing.Parser (fail, runParser)
import Text.Parsing.Parser.String (eof)

type StringBoomerang m = Boomerang m String

lit :: forall m r. (Monad m) => String -> StringBoomerang m r r
lit s =
  Boomerang {
      prs : const id <$> Parser.String.string s
    , ser : Serializer (pure <<< Tuple (s <> _))
  }

string :: forall m r. (Applicative m, Plus m) => String -> StringBoomerang m r (HCons String r)
string s =
  Boomerang {
      prs : (hCons <$> Parser.String.string s)
    , ser : Serializer ser
  }
 where
  ser (HCons s' t) =
    if s' == s
      then pure (Tuple (s <> _) t)
      else empty

oneOf :: forall m r. (Applicative m, Plus m) =>
                     Array Char ->
                     StringBoomerang m r (HCons Char r)
oneOf a =
  Boomerang {
      prs : (hCons <$> Parser.String.oneOf a)
    , ser : Serializer ser
  }
 where
  ser (HCons c t) =
    if c `elem` a
      then pure (Tuple (fromCharArray [c] <>  _) t)
      else empty

noneOf :: forall m r. (Applicative m, Plus m) => Array Char -> StringBoomerang m r (HCons Char r)
noneOf a =
  Boomerang {
      prs : (hCons <$> Parser.String.noneOf a)
    , ser : Serializer ser
  }
 where
  ser (HCons c t) =
    if not (c `elem` a)
      then pure (Tuple (fromCharArray [c] <>  _) t)
      else empty

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

manyOf :: forall m r. (Monad m, Plus m) =>
                      String ->
                      StringBoomerang m r (HCons String r)
manyOf a =
  pureBmg prs ser `compose` list (oneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = pure (hMap (fromFoldable <<< toCharArray) h)

many1Of :: forall m r. (Monad m, Plus m) => String -> StringBoomerang m r (HCons String r)
many1Of a =
  pureBmg prs ser `compose` cons `compose` oneOf a' `compose` list (oneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = pure (hMap (fromFoldable <<< toCharArray) h)

manyNoneOf :: forall m r. (Monad m, Plus m) => String -> StringBoomerang m r (HCons String r)
manyNoneOf a =
  pureBmg prs ser <<< list (noneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = pure (hMap (fromFoldable <<< toCharArray) h)

many1NoneOf :: forall m r. (Monad m, Plus m) => String -> StringBoomerang m r (HCons String r)
many1NoneOf a =
  pureBmg prs ser `compose` cons `compose` noneOf a' `compose` list (noneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = pure (hMap (fromFoldable <<< toCharArray) h)

digits :: forall m r. (Monad m, Plus m) => StringBoomerang m r (HCons String r)
digits = many1Of "0123456789"

int :: forall m r. (Applicative m) => Boomerang m String r (HCons Int r)
int =
  Boomerang
    { prs: prs
    , ser: ser
    }
 where
  ser =
    Serializer s
   where
    s (HCons i t) = pure (Tuple ((show i <> _)) t)

  prs = hCons <$> do
    a <- (Array.some <<<
          Parser.String.oneOf <<<
          toCharArray $ "0123456789")
    let s = fromCharArray a
    case Int.fromString s of
      Nothing -> fail ("Integer parsing error: " <> s)
      Just v -> pure v

parse :: forall a m. StringBoomerang m HNil (HCons a HNil) -> String -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s (do
    r <- b.prs
    -- we have to consume whole input
    eof
    pure r))
  pure (hHead (f hNil))

serialize :: forall a m. (Monad m) => StringBoomerang m HNil (HCons a HNil) -> a -> m String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  pure (f "")
