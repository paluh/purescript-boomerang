module Text.Boomerang.Strings where

import Data.Array.NonEmpty (NonEmpty, toArray)
import Data.Foldable (class Foldable, elem, foldMap)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String (fromChar, toCharArray)
import Prelude (compose, const, id, (<$>), (<>), (<<<), (==))
import Text.Boomerang.Combinators (list, pure)
import Text.Boomerang.HStack (hCons, HCons(..), hMap)
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

oneOf :: forall r. NonEmpty Char -> StringBoomerang r (HCons Char r)
oneOf na =
  Boomerang {
      prs : (hCons <$> Text.Parsing.Parser.String.oneOf a)
    , ser : Serializer ser
  }
 where
  a = toArray na
  ser (HCons c t) =
    if c `elem` a
      then Just (Tuple (fromChar c <>  _) t)
      else Nothing

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar

manyOf :: forall r. NonEmpty Char -> StringBoomerang r (HCons String r)
manyOf na =
  pure prs ser `compose` list (oneOf na)
 where
  prs = hMap fromCharList
  ser h = Just (hMap (fromFoldable <<< toCharArray) h)
