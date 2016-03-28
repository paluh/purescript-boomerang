module Test.Main where

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Control.Error.Util (hush)
import Data.Maybe (Maybe)
import Data.List (List(..), (:))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (bind, compose, const, return, show, Unit, unit, (<>), ($), (<<<))
import Text.Boomerang.HStack (hCons, HCons, hHead, HNil, hNil, hSingleton)
import Text.Boomerang.Combinators (cons, list, nil)
import Text.Boomerang.Prim (Boomerang(..), runSerializer)
import Text.Boomerang.Routing ((</>))
import Text.Boomerang.String (int, lit, string, StringBoomerang)
import Text.Parsing.Parser (runParser)
import Unsafe.Coerce (unsafeCoerce)

parse :: forall a r. StringBoomerang r (HCons a r) -> String -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s b.prs)
  return (hHead (f todo))

todo :: forall a. a
todo = unsafeCoerce unit

serialize :: forall a t. StringBoomerang HNil (HCons a t) -> (HCons a t) -> Maybe String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser s
  return (f "")

main :: forall a. Eff ( console :: CONSOLE | a) Unit
main = do
  let foo = string "foo"
      bar = string "bar"
      fooBar = (cons `compose` foo `compose` cons `compose` bar `compose` nil)
      -- fooOrBar = foo <> bar
      -- fooOrBarList = list fooOrBar
      -- fooOrBarList = list fooBar

  log (show (parse foo "foo"))
  log (show (parse bar "foo"))
  log (show (parse fooBar "foobar"))
  log (show (parse fooBar "barfoo"))
  -- log (show (parse fooOrBar "bar"))
  -- log (show (parse fooOrBar "foo"))
  -- log (show (serialize fooBar (hSingleton ("foo" : "bar" : Nil))))

  -- log (show (parse fooOrBarList "foofoofoobarfoobar"))
  -- log (show (serialize fooOrBarList (hSingleton ("bar": "foo": "foo" : "bar" : Nil))))

  -- log (show (parse (list (t <> f)) "testtesttestfesttest"))
  -- log (show (parse (cons `compose` digits `compose` cons `compose` t) "8889test"))
  -- log (show (parse (cons `compose` int `compose` lit "/" `compose` cons `compose` int) "8889/999"))
  -- log (show (parse (int </> int) "8889/999"))
