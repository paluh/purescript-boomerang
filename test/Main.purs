module Test.Main where

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Control.Error.Util (hush)
import Data.Maybe (Maybe)
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (bind, compose, const, return, show, Unit, (<>), ($), (<<<))
import Text.Boomerang.HStack (hCons, HCons, hHead, HNil, hNil)
import Text.Boomerang.Combinators (cons, list, nil)
import Text.Boomerang.Prim (Boomerang(..), runSerializer)
import Text.Boomerang.String (int, lit, manyOf, string, StringBoomerang)
import Text.Parsing.Parser (runParser)

parse :: forall a. StringBoomerang HNil (HCons a HNil) -> String -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s b.prs)
  return (hHead (f hNil))

serialize :: forall a t. StringBoomerang HNil (HCons a t) -> (HCons a t) -> Maybe String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser s
  return (f "")

main :: forall a. Eff ( console :: CONSOLE | a) Unit
main = do
  let t = (string "test" :: forall t. Boomerang String t (HCons String t))
      f = (string "fest" :: forall t. Boomerang String t (HCons String t))
      digits = (manyOf "0123456789")

  log (show (parse t "test"))
  log (show (parse f "test"))
  log (show (parse (f <> t) "test"))
  log (show (parse (cons `compose` t `compose` cons `compose` f `compose` nil) "testfesttest"))
  log (show (parse (list (t <> f)) "testtesttestfesttest"))
  log (show (parse (cons `compose` digits `compose` cons `compose` t `compose` nil) "8889test"))
  log (show (parse (cons `compose` int `compose` lit "/" `compose` cons `compose` int `compose` nil) "8889/999"))
