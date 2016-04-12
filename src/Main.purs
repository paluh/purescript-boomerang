module Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Error.Util (hush)
import Data.Generic (class Generic, GenericSpine(..), gEq, gShow, toSpine)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (bind, class Eq, class Show,
                return, show, Unit, (<>), (==), (<<<))
import Test.Unit (test, runTest, TIMER)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, equal)
import Text.Boomerang.HStack (class HList, hArg, hCons, HCons(..),
                              hHead, HNil, hNil, hSingleton, hTop2, HTop2)
import Text.Boomerang.Combinators (cons, nil, opt, pure)
import Text.Boomerang.Prim (Boomerang(..), runSerializer)
import Text.Boomerang.Routing ((</>), gRoute)
import Text.Boomerang.String (int, lit, string, StringBoomerang)
import Text.Parsing.Parser (runParser)
import Type.Proxy (Proxy(..))

serialize :: forall a. StringBoomerang HNil (HCons a HNil) -> a -> Maybe String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  return (f "")

data SimplePositionalValue = SimplePositionalValue Int
derive instance genericSimplePositionalValues :: Generic SimplePositionalValue

data SimplePositionalValues = SimplePositionalValues Int Boolean
derive instance genericSimplePositionalValue :: Generic SimplePositionalValues

data IntRecord = IntRecord { id :: Int }
derive instance genericIntRoute :: Generic IntRecord

data IntBoolRecord = IntBoolRecord { id :: Int, bool :: Boolean }
derive instance genericBooleanIntRoute :: Generic IntBoolRecord


main = do
  let booleanRoute = gRoute (Proxy :: Proxy Boolean)
      intRoute = gRoute (Proxy :: Proxy Int)
  log (show (serialize booleanRoute true))
  log (show (serialize intRoute 8))

  let route = gRoute (Proxy :: Proxy SimplePositionalValue)
      obj = SimplePositionalValue 8
  log (show (serialize route obj))

  let route = gRoute (Proxy :: Proxy SimplePositionalValue)
      obj = SimplePositionalValue 8
  log (show (serialize route obj))

  -- let intRecordRoute = gRoute (Proxy :: Proxy IntRecord)
  -- log (show (serialize intRecordRoute (Just (IntRecord { id : 8 }))))
