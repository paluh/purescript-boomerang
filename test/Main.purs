module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Error.Util (hush)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (bind, class Eq, class Show,
                return, Unit, (<>), (==), (<<<))
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

parse :: forall a. StringBoomerang HNil (HCons a HNil) -> String -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s b.prs)
  return (hHead (f hNil))

serialize :: forall a. StringBoomerang HNil (HCons a HNil) -> a -> Maybe String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser (hSingleton s)
  return (f "")

data ProfileViewMode = Compact | Extended
derive instance genericProfileViewMode :: Generic ProfileViewMode

data Profile = Profile
  { id :: Int
  , view :: ProfileViewMode
  }
derive instance genericProfile :: Generic Profile

instance showProfile :: Show Profile where
  show = gShow

instance eqProfile :: Eq Profile where
  eq = gEq

profileR :: forall r. (HList r) => StringBoomerang r (HCons Profile r)
profileR =
  pR <<< lit "profile" </> int </> pvR <<< opt (lit "/")
 where
  -- Profile route
  pR :: forall t. StringBoomerang (HTop2 Int ProfileViewMode t) (HCons Profile t)
  pR = pure (hArg (hArg hCons) (\i v -> Profile { id : i, view : v}))
            (\(HCons (Profile p) t) -> Just (hTop2 p.id p.view t))

  -- ProfileView route
  pvR :: forall t. StringBoomerang t (HCons ProfileViewMode t)
  pvR =
    vb <<< (string "compact" <> string "extended")
   where
    vb = pure (hArg (hCons) (\s -> if s == "compact" then Compact else Extended))
              ser
    ser (HCons Compact t) = Just (hCons "compact" t)
    ser (HCons Extended t) = Just (hCons "extended" t)

data BooleanIntRoute = BooleanIntRoute
  { extended :: Boolean
  , id :: Int
  }
derive instance genericBooleanIntRoute :: Generic BooleanIntRoute

data SimplePositionalValue = SimplePositionalValue Int
derive instance genericSimplePositionalValue :: Generic SimplePositionalValue
instance eqSimplePositionalValue :: Eq SimplePositionalValue where
  eq = gEq
instance showSimplePositionalValue :: Show SimplePositionalValue where
  show = gShow

data SimplePositionalValues = SimplePositionalValues Int Boolean Int
derive instance genericSimplePositionalValues :: Generic SimplePositionalValues
instance eqSimplePositionalValues :: Eq SimplePositionalValues where
  eq = gEq
instance showSimplePositionalValues :: Show SimplePositionalValues where
  show = gShow

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest do
  test "String routes parsing" do
    let foo = (string "foo" :: forall r. StringBoomerang r (HCons String r))
        bar = (string "bar" :: forall r. StringBoomerang r (HCons String r))
    equal (Just "foo") (parse foo "foo")
    equal Nothing (parse foo "bar")

  test "Basic composition with list aggregation" do
    let fooBar = (cons <<< string "foo" <<< cons <<< string "bar" <<< nil)
    test "parsing" do
      equal (Just ("foo":"bar":Nil)) (parse fooBar "foobar")
      equal Nothing (parse fooBar "barfoo")
    test "serialization" do
      equal (Just "foobar") (serialize fooBar ("foo" : "bar" : Nil))
      equal Nothing (serialize fooBar ("foo" : "bar" : "baz" : Nil))

  test "Basic alternatives" do
    let fooOrBar = string "foo" <> string "bar"
    test "parsing" do
      equal (Just "bar") (parse fooOrBar "bar")
      equal (Just "foo") (parse fooOrBar "foo")
    test "serialization" do
      equal (Just "foo") (serialize fooOrBar "foo")
      equal (Just "bar") (serialize fooOrBar "bar")

  test "Profile routes" do
    let profile = Profile {id: 20, view: Compact}
    equal (Just profile) (parse profileR "profile/20/compact/")
    assert "profile route parsing without trailing slash"
      (parse profileR "profile/20/compact/" == Just profile)
    equal (Just "profile/20/compact/") (serialize profileR profile)

  test "gRoute handles constructor with single, primitive value" do
    let route = gRoute (Proxy :: Proxy SimplePositionalValue)
        obj = SimplePositionalValue 8
    equal (Just "/8") (serialize route obj)
    equal (Just obj) (parse route "/8")

  test "gRoute handles construtor with multiple, primitive values" do
    let route = gRoute (Proxy :: Proxy SimplePositionalValues)
        obj = SimplePositionalValues 8 true 9
    equal (Just "/8/on/9") (serialize route obj)
    equal (Just obj) (parse route "/8/on/9")

  -- test "gRoute handles construtor with simple positional args" do
  --   let route = gRoute (Proxy :: Proxy SimplePositionalValues)
  --       obj = SimplePositionalValues 8 true
  --   equal (Just "/on/10") (serialize route (Just obj))

  -- test "gBoomerang handles BooleanIntRoute" do
  --   let b = BooleanIntRoute { extended : true, id : 10 }
  --       bRoute = gRoute (Proxy :: Proxy BooleanIntRoute)
  --   equal (Just "/on/10") (serialize bRoute (Just b))

