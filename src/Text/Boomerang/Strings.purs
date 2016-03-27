module Text.Boomerang.Strings where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (const, (<$>), (<>), (<<<))
import Text.Boomerang.Combinators (list)
import Text.Boomerang.Prim (Boomerang(..), Serializer(..))
import Text.Parsing.Parser.String (string)

type StringBoomerang = Boomerang String

lit :: forall r. String -> StringBoomerang r r
lit s =
  Boomerang {
      prs : const Just <$> string s
    , ser : Serializer (Just <<< Tuple (s <> _))
  }

