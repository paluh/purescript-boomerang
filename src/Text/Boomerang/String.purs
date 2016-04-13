module Text.Boomerang.String where

import Data.Foldable (class Foldable, elem, foldMap)
import Data.Int (fromString)
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String (fromChar, toCharArray)
import Prelude (compose, const, id, show, Unit, (<$>), (<>), (<<<), (==))
import Text.Boomerang.Combinators (cons, list, maph, pure)
import Text.Boomerang.HStack (class HList, hCons, HCons(..), hMap)
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Parsing.Parser.String

type StringBoomerang = Boomerang String

lit :: forall r. String -> StringBoomerang r r
lit s =
  Boomerang {
      prs : const id <$> Text.Parsing.Parser.String.string s
    , ser : Serializer (Just <<< Tuple (s <> _))
  }

string :: forall r. String -> StringBoomerang r (HCons String r)
string s =
  Boomerang {
      prs : (hCons <$> Text.Parsing.Parser.String.string s)
    , ser : Serializer ser
  }
 where
  ser (HCons s' t) =
    if s' == s
      then Just (Tuple (s <> _) t)
      else Nothing

oneOf :: forall r. Array Char -> StringBoomerang r (HCons Char r)
oneOf a =
  Boomerang {
      prs : (hCons <$> Text.Parsing.Parser.String.oneOf a)
    , ser : Serializer ser
  }
 where
  ser (HCons c t) =
    if c `elem` a
      then Just (Tuple (fromChar c <>  _) t)
      else Nothing

noneOf :: forall r. Array Char -> StringBoomerang r (HCons Char r)
noneOf a =
  Boomerang {
      prs : (hCons <$> Text.Parsing.Parser.String.noneOf a)
    , ser : Serializer ser
  }
 where
  ser (HCons c t) =
    if not (c `elem` a)
      then Just (Tuple (fromChar c <>  _) t)
      else Nothing

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar

manyOf :: forall r. String -> StringBoomerang r (HCons String r)
manyOf a =
  pure prs ser `compose` list (oneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = Just (hMap (fromFoldable <<< toCharArray) h)

many1Of :: forall r. String -> StringBoomerang r (HCons String r)
many1Of a =
  pure prs ser `compose` cons `compose` oneOf a' `compose` list (oneOf a')
 where
  a' = toCharArray a
  prs = hMap fromCharList
  ser h = Just (hMap (fromFoldable <<< toCharArray) h)

digits :: forall r. StringBoomerang r (HCons String r)
digits = many1Of "0123456789"

-- int :: forall r. Unit -> StringBoomerang r (HCons Int r)
int :: forall r. (HList r) => Boomerang String r (HCons Int r)
int =
  maph intPrs intSer `compose` digits
 where
  intPrs :: String -> Int
  intPrs s = fromMaybe 0 (fromString s)

  intSer :: Int -> Maybe String
  intSer i = Just (show i)
