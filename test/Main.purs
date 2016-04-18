module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Prelude (bind, class Eq, class Show, compose, Unit, (<>), (==), (<<<))
import Test.Unit (test, runTest, TIMER)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert, equal)
import Text.Boomerang.HStack (class HList, hArg, hCons, HCons(..), hTop2, HTop2)
import Text.Boomerang.Combinators (cons, listSep, nil, opt, pure)
import Text.Boomerang.String (int, lit, parse, serialize, string, StringBoomerang)

join :: forall a b c. StringBoomerang b c -> StringBoomerang a b -> StringBoomerang a c
join b1 b2 = b1 `compose` lit "/" `compose` b2

data ProfileViewMode = Compact | Extended
derive instance genericProfileViewMode :: Generic ProfileViewMode

infixl 6 join as </>

data Profile =
  Profile {
      id :: Int
    , view :: ProfileViewMode
  }
derive instance genericProfile :: Generic Profile

instance showProfile :: Show Profile where
  show = gShow

instance eqProfile :: Eq Profile where
  eq = gEq

profileR :: forall r. (HList r) => StringBoomerang r (HCons Profile r)
profileR =
  lit "profile" </> pR `compose` int </> pvR `compose` opt (lit "/")
 where
  -- Profile route
  pR :: forall t. StringBoomerang (HTop2 Int ProfileViewMode t) (HCons Profile t)
  pR = pure (hArg (hArg hCons) (\i v -> Profile { id : i, view : v}))
            (\(HCons (Profile p) t) -> Just (hTop2 p.id p.view t))

  -- ProfileView route
  pvR :: forall t. StringBoomerang t (HCons ProfileViewMode t)
  pvR =
    vb `compose` (string "compact" <> string "extended")
   where
    vb = pure (hArg (hCons) (\s -> if s == "compact" then Compact else Extended))
              ser
    ser (HCons Compact t) = Just (hCons "compact" t)
    ser (HCons Extended t) = Just (hCons "extended" t)

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest do
  test "String parsing" do
    let foo = (string "foo" :: forall r. StringBoomerang r (HCons String r))
        bar = (string "bar" :: forall r. StringBoomerang r (HCons String r))
    equal (Just "foo") (parse foo "foo")
    equal Nothing (parse foo "bar")

  test "`parse` function consume whole input" do
    let foo = (string "foo" :: forall r. StringBoomerang r (HCons String r))
    equal Nothing (parse foo "foo-and-something-more")

  test "Basic composition with list aggregation" do
    let fooBar = (cons `compose` string "foo" `compose` cons `compose` string "bar" `compose` nil)
    test "parsing" do
      equal (Just ("foo":"bar":Nil)) (parse fooBar "foobar")
      equal Nothing (parse fooBar "barfoo")
    test "serialization" do
      equal (Just "foobar") (serialize fooBar ("foo" : "bar" : Nil))
      equal Nothing (serialize fooBar ("foo" : "bar" : "baz" : Nil))

  test "listSep combinator for non empty list" do
    let intList = (listSep int (lit ","))
    equal (Just (1:2:3:4:5:Nil)) (parse intList "1,2,3,4,5")
    equal (Just "1,2,3,4,5") (serialize intList (1:2:3:4:5:Nil))

  test "listSep combinator for singleton list" do
    let intList = (listSep int (lit ","))
    equal (Just (1:Nil)) (parse intList "1")
    equal (Just "1") (serialize intList (1:Nil))

  test "listSep combinator for empty list" do
    let intList = (listSep int (lit ","))
    equal (Just (Nil)) (parse intList "")
    equal (Just "") (serialize intList (Nil))

  test "Basic alternatives" do
    let fooOrBar = string "foo" <> string "bar"
    test "parsing" do
      equal (Just "bar") (parse fooOrBar "bar")
      equal (Just "foo") (parse fooOrBar "foo")
    test "serialization" do
      equal (Just "foo") (serialize fooOrBar "foo")
      equal (Just "bar") (serialize fooOrBar "bar")

  test "Alternatives with prefixes" do
    let fooOrBar =
      (lit "foo-prefix" <<< string "foo") <>
      (lit "bar-prefix" <<< string "bar")
    test "parsing" do
      equal (Just "bar") (parse fooOrBar "bar-prefixbar")
      equal (Just "foo") (parse fooOrBar "foo-prefixfoo")
    test "serialization" do
      equal (Just "foo-prefixfoo") (serialize fooOrBar "foo")
      equal (Just "bar-prefixbar") (serialize fooOrBar "bar")

  test "Multiple choices with string prefixes" do
    let fooOrBar =
      string "/" <<< lit "foo-prefix" <>
      string "/" <<< lit "bar-prefix"
    test "parsing" do
      equal (Just "/") (parse fooOrBar "/foo-prefix")
      equal (Just "/") (parse fooOrBar "/bar-prefix")

  test "Multiple choices with literal prefixes" do
    let fooOrBarOrBaz =
      lit "/" <<< lit "foo-prefix" <<< string "foo" <>
      lit "/" <<< lit "bar-prefix" <<< string "bar" <>
      lit "/" <<< lit "baz-prefix" <<< string "baz"
    test "parsing" do
      equal (Just "bar") (parse fooOrBarOrBaz "/bar-prefixbar")
      equal (Just "foo") (parse fooOrBarOrBaz "/foo-prefixfoo")
      equal (Just "baz") (parse fooOrBarOrBaz "/baz-prefixbaz")
    test "serialization" do
      equal (Just "/foo-prefixfoo") (serialize fooOrBarOrBaz "foo")
      equal (Just "/bar-prefixbar") (serialize fooOrBarOrBaz "bar")
      equal (Just "/baz-prefixbaz") (serialize fooOrBarOrBaz "baz")

  test "Profile routes" do
    let profile = Profile {id: 20, view: Compact}
    equal (Just profile) (parse profileR "profile/20/compact/")
    assert "profile route parsing without trailing slash"
      (parse profileR "profile/20/compact/" == Just profile)
    equal (Just "profile/20/compact/") (serialize profileR profile)
